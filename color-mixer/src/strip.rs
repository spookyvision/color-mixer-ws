use std::{
    fmt::Debug,
    hash::Hash,
    ops::{Deref, DerefMut},
};

use derive_more::AsRef;
use palette::{IntoColor, Luv, Mix, Srgb};

pub type Srgb8 = palette::rgb::Rgb<palette::encoding::Srgb, u8>;

use derive_more::{Deref, DerefMut, From, Into};

#[derive(Clone, PartialEq, From, Into, Deref, DerefMut, Debug, Serialize, Deserialize)]
pub struct Wrap(Srgb8);

impl Default for Segment {
    fn default() -> Self {
        Self::new(
            1,
            false,
            Srgb8::new(255, 150, 0),
            Srgb8::new(255, 10, 220),
            2000,
        )
    }
}

impl Hash for Wrap {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.red.hash(state);
        self.green.hash(state);
        self.blue.hash(state);
    }
}

#[derive(PartialEq, Clone, Hash, Debug, AsRef, Serialize, Deserialize)]
pub struct Segment {
    length: usize,
    bgr: bool,
    colors: [Wrap; 2],
    chill_ms: u32,
}

impl Segment {
    pub fn new(length: usize, bgr: bool, c1: Srgb8, c2: Srgb8, chill_ms: u32) -> Self {
        Self {
            length,
            bgr,
            colors: [Wrap(c1), Wrap(c2)],
            chill_ms,
        }
    }

    pub fn mix(&self, mut t: f32) -> Srgb8 {
        let mut c1: Luv = self.color_1().into_format().into_color();
        let mut c2: Luv = self.color_2().into_format().into_color();
        if t >= 0.5 {
            (c1, c2) = (c2, c1);
            t -= 0.5;
        }
        t = simple_easing::sine_in_out(t * 2.0);

        let res = c1.mix(c2, t);
        // TODO: bgr
        let res: Srgb = res.into_color();
        res.into_format()
    }
    pub fn color_at(&self, at_millis: u32) -> Srgb8 {
        let wrapped = (at_millis % self.chill_ms) as f32;
        let chill = self.chill_ms as f32;
        let t = wrapped / chill;
        self.mix(t)
    }

    pub fn chill_ms(&self) -> u32 {
        self.chill_ms
    }

    pub fn color_1(&self) -> &Srgb8 {
        &self.colors[0]
    }

    pub fn color_2(&self) -> &Srgb8 {
        &self.colors[1]
    }
}

#[cfg(feature = "wasm")]
mod imp {
    use chrono::{DateTime, Utc};
    pub struct Control {
        start: DateTime<Utc>,
        now: DateTime<Utc>,
    }
    impl Control {
        pub fn new() -> Self {
            let now = Utc::now();
            Self { start: now, now }
        }

        pub fn tick(&mut self) -> u32 {
            self.now = Utc::now();
            let dt = self
                .now
                .signed_duration_since(self.start)
                .to_std()
                .unwrap()
                .as_millis();
            dt as u32 // :&
        }

        pub fn set_now(&mut self, now: DateTime<Utc>) {
            self.now = now;
        }
    }
}

#[cfg(not(feature = "wasm"))]
mod imp {
    pub struct Control {
        // start: DateTime<Utc>,
        // now: DateTime<Utc>,
        start: u32,
        now: u32,
    }
    impl Control {
        pub fn new() -> Self {
            // let now = Utc::now();
            let now = 0;
            Self { start: now, now }
        }

        pub fn set_now(&mut self, now: u32) {
            self.now = now;
        }
    }
}

pub use imp::Control;

use serde::{Deserialize, Serialize};

#[derive(PartialEq, Hash, Clone, Debug, Serialize, Deserialize)]
pub struct State {
    segments: Vec<Segment>,
}

impl State {
    pub fn new(segments: impl Iterator<Item = Segment>) -> Self {
        Self {
            segments: segments.collect(),
        }
    }

    pub fn new_empty() -> Self {
        Self { segments: vec![] }
    }
}

impl Deref for State {
    type Target = Vec<Segment>;

    fn deref(&self) -> &Self::Target {
        &self.segments
    }
}

impl DerefMut for State {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.segments
    }
}
