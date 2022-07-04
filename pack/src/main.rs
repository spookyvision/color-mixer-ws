use itertools::Itertools;
use lazy_static::lazy_static;
use quote::quote;
use std::{
    collections::HashMap,
    env,
    fs::File,
    io::Write,
    path::{self, Path},
    process::Command,
};

lazy_static! {
    static ref MIME: HashMap<String, &'static str> = [
        ("html", "text/html"),
        ("js", "text/javascript"),
        ("wasm", "application/wasm"),
        ("css", "text/css")
    ]
    .iter()
    .copied()
    .map(|(k, v)| (k.to_string(), v))
    .collect();
}

trait Slash {
    fn slashed(&self) -> String;
}

impl Slash for Path {
    fn slashed(&self) -> String {
        self.components()
            .map(|comp| comp.as_os_str().to_string_lossy())
            .collect::<Vec<_>>()
            .join("/")
    }
}
fn walk(root: &Path, this: &Path, out: &mut File, config: &Config) -> anyhow::Result<()> {
    let paths = std::fs::read_dir(this)?;

    for p in paths {
        let p = p?;
        let md = p.metadata()?;
        if md.is_dir() {
            let _ = walk(root, p.path().as_path(), out, config);
        } else if md.is_file() {
            if let Ok(relative) = p.path().strip_prefix(root) {
                dbg!(relative);
                let extension = p
                    .path()
                    .extension()
                    .map(|os_s| os_s.to_string_lossy())
                    .unwrap_or("".into())
                    .to_string();

                if extension == "gz" || p.path().ends_with(".gitkeep") {
                    dbg!("skipping", relative);
                    continue;
                }
                let slashed = relative.slashed();
                let web_path = format!("/{slashed}").replace("index.html", ""); // yolo

                let mime = MIME.get(&extension).unwrap_or(&"application/octet-stream");
                dbg!("doing", p.path(), mime);
                if let Err(e) = Command::new(&config.gzip_compressor)
                    .args(config.gzip_args.split(" "))
                    .arg(p.path())
                    .output()
                {
                    eprintln!("!! {e:?}");
                }
                let mut compressed_absolute = p.path().canonicalize()?.slashed();

                for (search, replace) in config.path_replacements.iter() {
                    compressed_absolute = compressed_absolute.replace(search, &replace);
                }
                compressed_absolute = compressed_absolute + ".gz";

                let tokens = quote! {
                    .handler(Handler::new(#web_path , Method::Get, |_| {
                        let data = include_bytes!(
                            #compressed_absolute
                        );
                        resp(data.as_slice(), #mime)
                    }))?
                };
                dbg!(tokens.to_string());
                out.write(tokens.to_string().as_bytes())?;
            }
        }
    }
    Ok(())
}

#[derive(Debug)]
struct Config {
    path_replacements: HashMap<String, String>,
    gzip_compressor: String,
    gzip_args: String,
}
fn main() -> anyhow::Result<()> {
    // env and process things under wsl are fuxored

    // "\\\\?\\C:/\\:/mnt/c"
    let supplied_path_replacements: Vec<&str> = option_env!("PACK_PATH_REPLACEMENTS")
        .unwrap_or_default()
        .split(":")
        .collect();

    let mut path_replacements: HashMap<String, String> = HashMap::default();

    let cs = supplied_path_replacements.chunks_exact(2);
    for spr in cs {
        let (k, v) = (spr[0].to_string(), spr[1].to_string());
        path_replacements.insert(k, v);
    }

    let config = Config {
        path_replacements,
        gzip_compressor: option_env!("COMPRESSOR").unwrap_or("gzip").to_string(),
        gzip_args: option_env!("GZIP_ARGS").unwrap_or("-9 -k").to_string(),
    };

    let mut args = env::args();
    args.next();
    let dest_path = args.next().unwrap();
    let dest_path = Path::new(&dest_path);

    let root = args.next().unwrap_or("../mixer-dioxus/dist".to_string());
    let root = Path::new(&root);

    let mut out = File::create(dest_path)?;
    out.write("ServerRegistry::new()".as_bytes())?;
    walk(root, root, &mut out, &config)?;
    Ok(())
}
