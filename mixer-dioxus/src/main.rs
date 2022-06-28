use std::rc::Rc;

use color_mixer::strip::{Control, Segment, Srgb8, State};
use dioxus::{core::to_owned, prelude::*};
use gloo::timers::future::TimeoutFuture;
use palette::Srgb;

fn main() {
    wasm_logger::init(wasm_logger::Config::default());
    console_error_panic_hook::set_once();

    dioxus::web::launch(app);
}

const PRIMES: &[u32] = &[
    2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97,
];

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
    let seg = Segment::new(100, false, **c1, **c2, PRIMES[*prime_idx] * fac);
    let col = seg.color_at(*now);

    let dur = seg.chill_ms();

    cx.render(rsx!(div {
        class: "square",
        style: format_args!("background-color: #{:x}", col),
        "{dur}ms"
    }))
}

#[allow(non_snake_case)]
#[inline_props]
fn ColorInput(cx: Scope, val: UseState<Srgb8>) -> Element {
    cx.render(rsx! {
        input {
            r#type: "color",
            value: format_args!("#{:x}", **val),
            oninput: move |ev| {
                let color: Srgb8 = ev.value.parse().unwrap();
                val.set(color);

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
        ColorInput{val: c1.clone()}
        ColorInput{val: c2.clone()}
        Color2{prime_idx: *prime_idx, fac: *fac, now: *now, c1: c1.clone(), c2: c2.clone()}
    ))
}

#[allow(non_snake_case)]
#[inline_props]
fn Segments(cx: Scope, state: State, fac: UseState<String>, now: u32) -> Element {
    let fac: u32 = fac.get().parse().unwrap();

    let content = state.iter().enumerate().map(|(id, seg)| {
        rsx! {
            div {
                key: "seg-{id}",
                SegmentN{seg:seg.clone(), prime_idx: id+10, fac: fac, now: *now}}
        }
    });

    cx.render(rsx!(content))
}

fn app(cx: Scope) -> Element {
    let control = use_ref(&cx, || Control::new());

    let state = State::new(
        [
            Segment::new(
                1,
                false,
                Srgb8::new(255, 150, 0),
                Srgb8::new(255, 10, 120),
                100,
            ),
            Segment::new(
                1,
                false,
                Srgb8::new(166, 0, 255),
                Srgb8::new(2, 192, 192),
                100,
            ),
            Segment::new(
                1,
                false,
                Srgb8::new(20, 200, 141),
                Srgb8::new(200, 176, 20),
                100,
            ),
            Segment::new(
                1,
                false,
                Srgb8::new(200, 20, 30),
                Srgb8::new(200, 200, 10),
                100,
            ),
        ]
        .into_iter(),
    );

    let now = control.write().tick();
    let now = use_state(&cx, || now);

    let initial_val = "400".to_string();

    let chill_val = use_state(&cx, || initial_val.clone());

    to_owned![now, control];
    let now_too = now.clone();

    let _irish_setter = use_future(&cx, &control, |c| async move {
        let dat_now = c.with_mut(|c| c.tick());
        now_too.set(dat_now);
    });

    cx.render(rsx! (
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

             Segments {state: state.clone(), fac: chill_val.clone(), now: *now}


         }
    ))
}
