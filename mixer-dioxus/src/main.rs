use chrono::Utc;
use color_mixer::strip::{Control, Segment, Srgb8, State, Wrap, CHILLED};
use dioxus::{core::to_owned, prelude::*};
use fermi::{use_atom_state, use_read, Atom, AtomState};
use futures::StreamExt;
use gloo::timers::future::TimeoutFuture;
use indexmap::IndexMap;
use log::debug;

pub static STATE_ATOM: Atom<Option<SegMap>> = |_| None;

const DEBOUNCE_MS: u64 = 300;

type Res<T> = Result<T, Box<dyn std::error::Error>>;

fn main() {
    wasm_logger::init(wasm_logger::Config::default());
    console_error_panic_hook::set_once();
    dioxus::web::launch(App2);
}

mod canvas;

#[allow(non_snake_case)]
fn App2(cx: Scope) -> Element {
    let base_url = use_state(&cx, || env!("HARLOT_BOARD").to_string());

    cx.render(rsx! {
        form {
            input {
                r#type: "text",
                class: "override",
                name: "base_url_override",
                placeholder: "base url override",
                value: "{base_url}",
                oninput: move |ev| {
                    let val = ev.value.clone();
                    base_url.set(val);
                },
            }
        }
        AppServerSync { base_url: base_url.clone()}
    })
}

#[allow(non_snake_case)]
#[inline_props]
fn AppServerSync(cx: Scope, base_url: UseState<String>) -> Element {
    let update = use_coroutine(&cx, |mut rx: UnboundedReceiver<SegMap>| {
        to_owned![base_url];
        async move {
            let mut last_update = Utc::now();

            let inner = async move {
                while let Some(mut data) = rx.next().await {
                    loop {
                        match rx.try_next() {
                            Ok(None) => {
                                log::info!("shutting down updater");
                            }
                            Ok(Some(next_data)) => {
                                log::debug!("but wait, there's more!");
                                data = next_data;
                                continue;
                            }
                            Err(_e) => {
                                // channel has been drained
                                log::debug!("I can't believe it's not bu^Wmore!");
                                break;
                            }
                        }
                    }

                    let now = Utc::now();
                    let debounce_amount = std::time::Duration::from_millis(DEBOUNCE_MS);

                    let dt = now
                        .signed_duration_since(last_update)
                        .to_std()
                        .unwrap_or(debounce_amount);

                    if let Some(wait) = debounce_amount.checked_sub(dt) {
                        log::debug!("debounce: {wait:?}");
                        TimeoutFuture::new(wait.as_millis() as u32).await;
                    }

                    let latest_base_url = base_url.current();
                    let url = format!("{latest_base_url}data");
                    log::debug!("updating DATA at {url}");

                    let ser = serde_json::to_vec(&data)?;
                    let mut req = surf::post(url).body_bytes(&ser).await?;
                    let _loaded = req.body_bytes().await?;

                    last_update = Utc::now();
                }
                Ok(())
            };

            let res: Res<()> = inner.await;
            if let Err(e) = res {
                log::error!("someone went wrong: {e:?}");
            }
        }
    });

    cx.provide_context(UpdateState(update.to_owned()));

    cx.render(rsx!(AppOutestest {
        base_url: base_url.to_string()
    }))
}

#[allow(non_snake_case)]
#[inline_props]
fn Color2(
    cx: Scope,
    prime_idx: usize,
    fac: u32,
    now: u32,
    c1: UseState<Srgb8>,
    c2: UseState<Srgb8>,
) -> Element {
    let seg = Segment::new(1, false, **c1, **c2, *prime_idx, *fac, 0);
    let col = seg.color_at(*now);

    let pc: piet::Color = piet::Color::rgb8(col.red, col.green, col.blue);

    let size = 20f64;
    let xy = size + 3.;
    cx.render(rsx!(div {
        class: "square",
        style: format_args!("border: 2px dotted #{:x}", col),

        canvas::web::Canvas{
            color: pc.clone(),
            canvas::web::Circle{x: xy, y: xy, radius:size, color: pc.clone()}
        }
    }))
}

type SegMap = IndexMap<String, Segment>;

fn send_update(update: Option<UpdateState>, segments: SegMap) {
    if let Some(ref update) = update {
        update.0.send(segments);
    }
}

fn edit_segments(
    segments: &AtomState<Option<SegMap>>,
    update: Option<UpdateState>,
    mut wat: impl FnMut(&mut SegMap),
) {
    segments.modify(|segments| {
        let mut segments = segments.to_owned();
        if let Some(ref mut segments) = segments {
            wat(segments);

            send_update(update, segments.clone());
        }

        segments
    });
}

#[allow(non_snake_case)]
#[inline_props]
fn ChillInput(cx: Scope, segment_id: String, chill_idx: UseState<usize>) -> Element {
    let segments: &AtomState<Option<SegMap>> = use_atom_state(&cx, STATE_ATOM);
    let update: Option<UpdateState> = cx.consume_context::<UpdateState>();

    let max = CHILLED.len() - 1;
    cx.render(rsx! {
        input {
            r#type: "range",
            name: "chill_idx",
            value: "{chill_idx}",
            min: "0",
            max: "{max}",
            oninput: move |ev| {
               chill_idx.set(ev.value.clone().parse().unwrap_or(0));
               edit_segments(segments, update.clone(), |segments| {
                    if let Some(segment) = segments.get_mut(segment_id) {
                        segment.set_chill_idx(**chill_idx);
                    }
                });
            },
        }
    })
}

#[allow(non_snake_case)]
#[inline_props]
fn ColorInput(cx: Scope, segment_id: String, color_idx: usize, val: UseState<Srgb8>) -> Element {
    let segments: &AtomState<Option<SegMap>> = use_atom_state(&cx, STATE_ATOM);
    let update: Option<UpdateState> = cx.consume_context::<UpdateState>();

    to_owned![val];
    let val_too = val.clone();
    let val_three = val.clone();

    cx.render(rsx! {
        input {
            r#type: "color",
            value: format_args!("#{:x}", * val_too),
            oninput: move |ev| {
                let color: Srgb8 = ev.value.parse().unwrap();
                val_three.set(color);
                edit_segments(segments, update.clone(), |segments| {
                    if let Some(segment) = segments.get_mut(segment_id) {
                        segment.colors_mut()[*color_idx] = Wrap(color);
                    }
                });
            },
        }
    })
}

#[allow(non_snake_case)]
#[inline_props]
fn SegmentN(cx: Scope, seg: Segment, prime_idx: usize, fac: u32, now: u32) -> Element {
    let segments: &AtomState<Option<SegMap>> = use_atom_state(&cx, STATE_ATOM);

    let id = seg.to_uuid_string();
    let id_too = id.clone();

    let cms = segments
        .as_ref()
        .map(|ss| {
            ss.get(&id)
                .map(|s| format!("{:.2}", s.chill_ms() as f32 / 1000.))
        })
        .flatten();
    // let cms = segments.as_ref().map(|ss| 1);
    let update: Option<UpdateState> = cx.consume_context::<UpdateState>();
    let update_too = update.clone();
    let update_tooer = update.clone();

    let c1 = use_state(&cx, || seg.color_1().to_owned());
    let c2 = use_state(&cx, || seg.color_2().to_owned());
    let chill_idx = use_state(&cx, || seg.chill_idx());

    let len = use_state(&cx, || seg.length());

    // let dur_s = cms.as_ref().unwrap_or_else(|| &Some("?".to_string())).unwrap_or_else(|| "?".to_string());
    let dur_s = cms.unwrap_or_else(|| "?".to_string());

    cx.render(rsx!(
        div {
            class: "segment",
            h2 {"c0lors"}
            Color2{prime_idx: *prime_idx, fac: *fac, now: *now, c1: c1.clone(), c2: c2.clone()}
            ColorInput{segment_id: id.clone(), color_idx: 0, val: c1.clone()}
            ColorInput{segment_id: id.clone(), color_idx: 1, val: c2.clone()}
            ChillInput{segment_id: id.clone(), chill_idx:chill_idx.clone()}
            "{dur_s}sec"

            h2 {"num leds"}
            input {
                r#type: "range",
                name: "num_leds_r",
                value: "{len}",
                min: "1",
                max: "60",
                oninput: move |ev| {
                    let val = ev.value.clone().parse().unwrap_or(1);
                    len.set(val);
                    edit_segments(segments, update_too.clone(), |segments| {
                        if let Some(segment) = segments.get_mut(&id) {
                            segment.set_length(val);
                        }
                    });
                },
            }

            input {
                r#type: "number",
                name: "num_leds_n",
                value: "{len}",
                min: "1",
                max: "999",
                oninput: move |ev| {
                    let val = ev.value.clone().parse().unwrap_or(1);
                    len.set(val);

                    edit_segments(segments, update_tooer.clone(), |segments| {
                        if let Some(segment) = segments.get_mut(&id_too) {
                            segment.set_length(val);
                        }
                    });
                },
            }

            br {}

            button {
                onclick: move |_evt| edit_segments(segments, update.clone(),  |segments| {segments.remove(&seg.to_uuid_string());}),
                "delete"
            }
        }
    ))
}

#[derive(Clone)]
struct UpdateState(CoroutineHandle<SegMap>);

#[allow(non_snake_case)]
#[inline_props]
fn Segments(cx: Scope, fac: UseState<u32>, now: u32) -> Element {
    let global_segments = use_read(&cx, STATE_ATOM);

    let content = match global_segments {
        None => rsx!(div {"loading..."}),
        // None => return None,
        Some(segments) => {
            let inner = segments.iter().map(|(segment_id, seg)| {
                rsx! {
                    div {
                        key: "seg-{segment_id}",
                        SegmentN{seg:seg.clone(), prime_idx: seg.chill_idx(), fac: **fac, now: *now}}
                }
            });
            rsx!(div { inner })
        }
    };

    cx.render(rsx!(div { content }))
}

#[allow(non_snake_case)]
#[inline_props]
fn AppOutestest(cx: Scope, base_url: String) -> Element {
    let segments_state = use_atom_state(&cx, STATE_ATOM).to_owned();

    let _doberman: &UseFuture<()> = use_future(&cx, base_url, |base_url| async move {
        to_owned![base_url];
        let url = base_url;
        let inner = async move {
            debug!("load DATA from {url}");
            let url = format!("{url}data");

            let mut res = surf::get(url).await?;
            let body = res.body_bytes().await?;

            let loaded_segments: IndexMap<String, Segment> = serde_json::from_slice(&body)?;
            debug!("loaded {loaded_segments:?}");
            segments_state.set(Some(loaded_segments));
            Ok(())
        };
        let res: Res<()> = inner.await;
        if let Err(e) = res {
            log::error!("could not load data: {:?}", e);
        }
    });

    let segments: &AtomState<Option<SegMap>> = use_atom_state(&cx, STATE_ATOM);

    let content = match segments.get() {
        Some(segments) => rsx!(App {
            base_url: base_url.clone(),
            segments: segments.clone()
        }),
        None => rsx!(h2{"loading"}),
    };

    cx.render(content)
}

#[allow(non_snake_case)]
#[inline_props]
fn App(cx: Scope, base_url: String, segments: SegMap) -> Element {
    let control = use_ref(&cx, || Control::new());
    let global_segments: &AtomState<Option<SegMap>> = use_atom_state(&cx, STATE_ATOM);
    let update: Option<UpdateState> = cx.consume_context::<UpdateState>();
    let update_too = update.clone();
    let update_tooest = update.clone();

    let now = control.write().tick();
    let now = use_state(&cx, || now);

    let delta = use_state(&cx, || 0i64);
    let delta_too = delta.clone();
    let delta_est = delta.clone();

    let initial_val = segments
        .iter()
        .next()
        .map(|(_id, seg)| seg.chill_fac())
        .unwrap_or(500);
    let chill_val = use_state(&cx, || initial_val);
    let brightness_val = use_state(&cx, || 10u8);
    let brightness_val_too = brightness_val.clone();

    to_owned![delta, control];
    let control_too = control.clone();
    let control_tooest = control.clone();
    let now_too = now.clone();
    let _old_delta = 0;
    let _english_setter: &UseFuture<Res<_>> = use_future(&cx, &control, |c| async move {
        let dat_now = c.with_mut(|c| c.tick());
        let mut _new_delta = *delta;
        now_too.set(dat_now + (*delta as u32));
        Ok(())
    });

    let _irish_setter: &UseFuture<Res<()>> = use_future(&cx, base_url, |base_url| async move {
        loop {
            let url = format!("{base_url}now");
            debug!("load NOW from {url}");
            let req_start = Utc::now();
            let mut res = surf::get(url).await?;
            let req_duration = Utc::now().signed_duration_since(req_start);
            let text = res.body_string().await?;
            let latency_estimate = req_duration.num_milliseconds() / 2;
            debug!("rd {latency_estimate}");
            let server_now = text.parse::<i64>()? + latency_estimate;
            debug!("{server_now}");
            let ms_since_start = control_too.with(|c| c.ms_since_start());
            let delta_value = server_now - ms_since_start as i64;

            delta_too.with_mut(|set| *set = delta_value);

            TimeoutFuture::new(2_000).await;
        }
    });

    // TODO range + Default newtype]
    let default_chill_fac = 150;

    let mss = control_tooest.read().ms_since_start();

    let content = rsx! (
     div {
        style: "text-align: center;",
        h1 { "LED zeppelin" }
        p { "our time: {now}, mss: {mss}, delta: {delta_est}"}
        form {
            input {
                r#type: "range",
                name: "chill_val",
                value: "{chill_val}",
                min: "40",
                max: "1000",
                oninput: move |ev| {
                let val = ev.value.clone();
                let chill_fac = val.parse().unwrap_or(default_chill_fac);
                chill_val.set(chill_fac);
                edit_segments(global_segments, update.clone(), |segments| {
                    for (_id, segment) in segments.iter_mut() {
                        segment.set_chill_fac(chill_fac);
                    }
                });

            },
            }
            h3 { "chill: {chill_val}"}

            input {
                r#type: "range",
                name: "brightness",
                value: "{brightness_val}",
                min: "1",
                max: "128",
                oninput: move |ev| {
                let val = ev.value.clone();
                let brightness = val.parse().unwrap_or(10u8);
                brightness_val.set(brightness);
                edit_segments(global_segments, update_tooest.clone(), |segments| {
                    for (_id, segment) in segments.iter_mut() {
                        segment.set_brightness(brightness);
                    }
                });

                },
            }
        }
        h3 { "brightness: {brightness_val}"}


        Segments {fac: chill_val.clone(), now: **now}
        button {
            onclick: move |_evt| edit_segments(global_segments, update_too.clone(),  |segments| {
                let mut seg = Segment::default();
                seg.set_chill_fac(**chill_val);
                seg.set_brightness(*brightness_val_too);
                segments.insert(seg.to_uuid_string(), seg);

            }),
            "new"
        }
    }
    );
    cx.render(content)
}
