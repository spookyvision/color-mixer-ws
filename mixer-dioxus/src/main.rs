use std::{
    rc::Rc,
    sync::{mpsc, Mutex},
};

use color_mixer::strip::{Control, Segment, Srgb8, State, Wrap};
use dioxus::{core::to_owned, prelude::*};
use fermi::{use_atom_state, use_init_atom_root, use_read, use_set, Atom};
use gloo::timers::future::TimeoutFuture;
use indexmap::IndexMap;
use palette::{stimulus::IntoStimulus, Srgb};

pub static STATE_ATOM: Atom<Option<State>> = |_| None;

const BASE_URL: Option<&'static str> = Some("http://127.0.0.1:8081/");

fn main() {
    wasm_logger::init(wasm_logger::Config::default());
    console_error_panic_hook::set_once();
    dioxus::web::launch(AppOuter);
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
    let seg = Segment::new(100, false, **c1, **c2, *prime_idx, *fac);
    let col = seg.color_at(*now);

    let dur = seg.chill_ms();

    cx.render(rsx!(div {
        class: "square",
        style: format_args!("background-color: #{:x}", col),
        "{dur}ms"
    }))
}

#[derive(Debug, Clone)]
struct DState(Rc<Mutex<IndexMap<String, Segment>>>);

#[allow(non_snake_case)]
#[inline_props]
fn ColorInput(cx: Scope, segment_id: String, color_idx: usize, val: UseState<Srgb8>) -> Element {
    let segments = cx.use_hook(|_| cx.consume_context::<DState>());

    cx.render(rsx! {
        input {
            r#type: "color",
            value: format_args!("#{:x}", **val),
            oninput: move |ev| {
                let color: Srgb8 = ev.value.parse().unwrap();
                val.set(color);
                log::debug!("{segments:?}");
                if let Some(segments) = segments {
                    let mut segments = segments.0.lock().unwrap();
                    if let Some(segment) = segments.get_mut(segment_id) {
                        segment.colors_mut()[*color_idx] = Wrap(color);
                    }
                }
            },
        }
    })
}

#[allow(non_snake_case)]
#[inline_props]
fn SegmentN(cx: Scope, seg: Segment, prime_idx: usize, fac: u32, now: u32) -> Element {
    let c1 = use_state(&cx, || seg.color_1().to_owned());
    let c2 = use_state(&cx, || seg.color_2().to_owned());

    cx.render(rsx!(
        ColorInput{segment_id: seg.to_uuid_string(), color_idx: 0, val: c1.clone()}
        ColorInput{segment_id: seg.to_uuid_string(), color_idx: 1, val: c2.clone()}
        Color2{prime_idx: *prime_idx, fac: *fac, now: *now, c1: c1.clone(), c2: c2.clone()}
    ))
}

#[allow(non_snake_case)]
#[inline_props]
fn Segments(cx: Scope, state: State, fac: UseState<String>, now: u32) -> Element {
    let fac: u32 = fac.get().parse().unwrap();
    cx.use_hook(|_| {
        cx.provide_context(DState(Rc::new(Mutex::new(state.segments().clone()))));
    });

    let content = state.iter().map(|(segment_id, seg)| {
        rsx! {
            div {
                key: "seg-{segment_id}",
                SegmentN{seg:seg.clone(), prime_idx: seg.chill_idx(), fac: fac, now: *now}}
        }
    });

    cx.render(rsx!(div { content }))
}

fn AppOuter(cx: Scope) -> Element {
    let set_state = Rc::clone(use_set(&cx, STATE_ATOM));

    let state = State::new(
        [
            Segment::new(
                1,
                false,
                Srgb8::new(255, 150, 0),
                Srgb8::new(255, 10, 120),
                0,
                100,
            ),
            Segment::new(
                1,
                false,
                Srgb8::new(166, 0, 255),
                Srgb8::new(2, 192, 192),
                1,
                100,
            ),
            Segment::new(
                1,
                false,
                Srgb8::new(20, 200, 141),
                Srgb8::new(200, 176, 20),
                2,
                100,
            ),
            Segment::new(
                1,
                false,
                Srgb8::new(200, 20, 30),
                Srgb8::new(200, 200, 10),
                3,
                100,
            ),
        ]
        .into_iter(),
    );

    set_state(Some(state));
    cx.render(rsx!(App {}))
}

fn App(cx: Scope) -> Element {
    let state: &Option<State> = use_read(&cx, STATE_ATOM);
    let control = use_ref(&cx, || Control::new());

    let now = control.write().tick();
    let now = use_state(&cx, || now);

    let delta = use_state(&cx, || 0i32);

    let initial_val = "400".to_string();

    let chill_val = use_state(&cx, || initial_val.clone());

    to_owned![delta, control];
    let control_too = control.clone();
    let delta_too = delta.clone();
    let now_too = now.clone();
    let mut old_delta = 0;
    let _english_setter: &UseFuture<Result<_, Box<dyn std::error::Error>>> =
        use_future(&cx, &control, |c| async move {
            let dat_now = c.with_mut(|c| c.tick());
            let mut new_delta = *delta;
            now_too.set(dat_now + (*delta as u32));
            Ok(())
        });

    let local_now = now.clone();
    let _irish_setter: &UseFuture<Result<_, Box<dyn std::error::Error>>>;
    if let Some(base_url) = BASE_URL {
        _irish_setter = use_future(&cx, (), |_| async move {
            loop {
                let url = format!("{base_url}now");
                let mut res = surf::get(url).await?;
                let text = res.body_string().await?;
                let server_now: i32 = text.parse()?;
                let ms_since_start = control_too.with(|c| c.ms_since_start());
                let delta_value = server_now as i32 - ms_since_start as i32;

                delta_too.with_mut(|set| *set = delta_value);

                TimeoutFuture::new(1_000).await;
            }
            Ok(())
        });
    }

    let content = match state {
        None => rsx!(p { "loading..." }),
        Some(state) => rsx! (
             div {
                 style: "text-align: center;",
                 h1 { "Bisexual lighting controller" }
                 h3 { "(bisexuality optional)" }
                 p { "time: {now}"}
                 form {
                     input {
                         r#type: "range",
                         name: "chill_val",
                         value: "{chill_val}",
                         id: "chill_val",
                         min: "10",
                         max: "800",
                         oninput: move |ev| chill_val.set(ev.value.clone()),
                     }
                 }
                 p { "chill: {chill_val}"}

                 Segments {state: state.clone(), fac: chill_val.clone(), now: **now}


             }
        ),
    };
    cx.render(content)
}
