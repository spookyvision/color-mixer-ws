use std::{
    borrow::{Borrow, BorrowMut},
    cell::{RefCell, RefMut},
    ops::{Deref, DerefMut},
    rc::Rc,
    sync::{atomic::AtomicBool, mpsc, Condvar, Mutex},
};

use chrono::{Duration, Utc};
use color_mixer::strip::{Control, Segment, Srgb8, State, Wrap};
use dioxus::{core::to_owned, prelude::*};
use fermi::{use_atom_state, use_init_atom_root, use_read, use_set, Atom, AtomState};
use futures::StreamExt;
// use futures_channel::mpsc::{unbounded, UnboundedReceiver};
use gloo::timers::future::TimeoutFuture;
use indexmap::IndexMap;
use log::debug;
use palette::{stimulus::IntoStimulus, Srgb};

pub static STATE_ATOM: Atom<Option<SegMap>> = |_| None;

const BASE_URL: Option<&'static str> = Some(env!("HARLOT_BOARD"));
// const BASE_URL: Option<&'static str> = Some("http://127.0.0.1:8081/");

type Res<T> = Result<T, Box<dyn std::error::Error>>;

fn main() {
    wasm_logger::init(wasm_logger::Config::default());
    console_error_panic_hook::set_once();
    dioxus::web::launch(AppOutest);
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

type SegMap = IndexMap<String, Segment>;

fn edit_segments(
    segments: &AtomState<Option<SegMap>>,
    update: Option<UpdateState>,
    mut wat: impl FnMut(&mut SegMap),
) {
    // edit_segments(segments, update, |segments| {
    //     if let Some(segment) = segments.get_mut(segment_id) {
    //         segment.colors_mut()[*color_idx] = Wrap(color);
    //     }
    // });
    segments.modify(|segments| {
        let mut segments = segments.to_owned();
        if let Some(ref mut segments) = segments {
            wat(segments);

            if let Some(ref update) = update {
                update.0.send(segments.clone());
            }
        }

        segments
    });
}

#[allow(non_snake_case)]
#[inline_props]
fn ColorInput(cx: Scope, segment_id: String, color_idx: usize, val: UseState<Srgb8>) -> Element {
    let segments: &AtomState<Option<SegMap>> = use_atom_state(&cx, STATE_ATOM);

    to_owned![val];
    let val_too = val.clone();
    let val_three = val.clone();

    let update: Option<UpdateState> = cx.consume_context::<UpdateState>();

    cx.render(rsx! {
        input {
            r#type: "color",
            value: format_args!("#{:x}", * val_too),
            oninput: move |ev| {
                let color: Srgb8 = ev.value.parse().unwrap();
                val_three.set(color);
                    segments.modify(|segments|
                        {
                            let mut segments = segments.to_owned();
                            if let Some(ref mut segments) = segments {
                                if let Some(segment) = segments.get_mut(segment_id) {
                                    segment.colors_mut()[*color_idx] = Wrap(color);

                                    if let Some(ref update) = update {
                                        update.0.send(segments.clone());
                                    }
                                }
                            }

                            segments
                        }
                    );
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

#[derive(Clone)]
struct UpdateState(CoroutineHandle<SegMap>);

#[allow(non_snake_case)]
#[inline_props]
fn Segments(cx: Scope, fac: UseState<String>, now: u32) -> Element {
    let fac: u32 = fac.get().parse().unwrap();

    let global_segments = use_read(&cx, STATE_ATOM);

    let update = use_coroutine(&cx, |mut rx: UnboundedReceiver<SegMap>| async move {
        if let Some(base_url) = BASE_URL {
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
                            Err(data) => {
                                log::debug!("I can't believe it's not bu^Wmore!");
                                break;
                            } // channel has been drained
                        }
                    }

                    let now = Utc::now();
                    let debounce_amount = std::time::Duration::from_millis(2_000);

                    let dt = now
                        .signed_duration_since(last_update)
                        .to_std()
                        .unwrap_or(debounce_amount);

                    if let Some(wait) = debounce_amount.checked_sub(dt) {
                        log::debug!("debounce: {wait:?}");
                        TimeoutFuture::new(wait.as_millis() as u32).await;
                    }
                    log::debug!("updating!");

                    let url = format!("{base_url}data");

                    let ser = serde_json::to_vec(&data)?;
                    let mut req = surf::post(url).body_bytes(&ser).await?;
                    let _loaded = req.body_bytes().await;

                    last_update = Utc::now();
                }
                Ok(())
            };

            let res: Res<()> = inner.await;
        }
    });

    cx.provide_context(UpdateState(update.to_owned()));

    let content = match global_segments {
        None => rsx!(div {"loading..."}),
        Some(segments) => {
            let inner = segments.iter().map(|(segment_id, seg)| {
                rsx! {
                    div {
                        key: "seg-{segment_id}",
                        SegmentN{seg:seg.clone(), prime_idx: seg.chill_idx(), fac: fac, now: *now}}
                }
            });
            rsx!(div { inner })
        }
    };

    cx.render(rsx!(div { content }))
}

fn AppOutest(cx: Scope) -> Element {
    use_context_provider(&cx, || SegMap::new());
    cx.render(rsx!(AppOuter {}))
}
fn AppOuter(cx: Scope) -> Element {
    let set_state = Rc::clone(use_set(&cx, STATE_ATOM));

    if let Some(base_url) = BASE_URL {
        cx.spawn({
            async move {
                let inner = async move {
                    let url = format!("{base_url}data");
                    let mut res = surf::get(url).await?;
                    let body = res.body_bytes().await?;
                    let loaded_segments: IndexMap<String, Segment> = serde_json::from_slice(&body)?;
                    debug!("loaded {loaded_segments:?}");
                    set_state(Some(loaded_segments));
                    Ok(())
                };
                let res: Res<()> = inner.await;
            }
        });
    }

    cx.render(rsx!(App {}))
}

fn App(cx: Scope) -> Element {
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
    let _english_setter: &UseFuture<Res<_>> = use_future(&cx, &control, |c| async move {
        let dat_now = c.with_mut(|c| c.tick());
        let mut new_delta = *delta;
        now_too.set(dat_now + (*delta as u32));
        Ok(())
    });

    let local_now = now.clone();

    let _irish_setter: &UseFuture<Res<_>> = use_future(&cx, (), |_| async move {
        if let Some(base_url) = BASE_URL {
            loop {
                let url = format!("{base_url}now");
                let mut res = surf::get(url).await?;
                let text = res.body_string().await?;
                let server_now: i32 = text.parse()?;
                let ms_since_start = control_too.with(|c| c.ms_since_start());
                let delta_value = server_now as i32 - ms_since_start as i32;

                delta_too.with_mut(|set| *set = delta_value);

                TimeoutFuture::new(2_000).await;
            }
        }
        Ok(())
    });

    let content = rsx! (
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

         Segments {fac: chill_val.clone(), now: **now}


     }
    );
    cx.render(content)
}
