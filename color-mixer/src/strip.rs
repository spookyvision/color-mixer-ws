use std::{
    fmt::Debug,
    hash::Hash,
    ops::{Deref, DerefMut},
};

use indexmap::IndexMap;
use palette::{IntoColor, Luv, Mix, Srgb};

pub type Srgb8 = palette::rgb::Rgb<palette::encoding::Srgb, u8>;

use derive_more::{Deref, DerefMut, From, Into};

pub const CHILLED: &[u32] = &[
    7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97,
];
#[derive(Clone, PartialEq, From, Into, Deref, DerefMut, Debug, Serialize, Deserialize)]
pub struct Wrap(pub Srgb8);

impl Default for Segment {
    fn default() -> Self {
        Self::new(
            1,
            false,
            Srgb8::new(255, 150, 0),
            Srgb8::new(255, 10, 220),
            0,
            80,
            10,
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

#[derive(PartialEq, Clone, Hash, Debug, Serialize, Deserialize)]
pub struct Segment {
    uuid: Uuid,
    length: usize,
    bgr: bool,
    colors: [Wrap; 2],
    chill_idx: usize,
    chill_fac: u32,
    brightness: u8,
}

impl Segment {
    pub fn new_with_uuid(
        uuid: Uuid,
        length: usize,
        bgr: bool,
        c1: Srgb8,
        c2: Srgb8,
        chill_idx: usize,
        chill_fac: u32,
        brightness: u8,
    ) -> Self {
        Self {
            uuid,
            length,
            bgr,
            colors: [Wrap(c1), Wrap(c2)],
            chill_idx,
            chill_fac,
            brightness,
        }
    }

    pub fn new(
        length: usize,
        bgr: bool,
        c1: Srgb8,
        c2: Srgb8,
        chill_idx: usize,
        chill_fac: u32,
        brightness: u8,
    ) -> Self {
        Self::new_with_uuid(
            Uuid::new_v4(),
            length,
            bgr,
            c1,
            c2,
            chill_idx,
            chill_fac,
            brightness,
        )
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

    pub fn chill_ms(&self) -> u32 {
        self.chill_fac * CHILLED[self.chill_idx]
    }

    pub fn color_at(&self, at_millis: u32) -> Srgb8 {
        let wrapped = (at_millis % self.chill_ms()) as f32;
        let chill = self.chill_ms() as f32;
        let t = wrapped / chill;
        self.mix(t)
    }

    pub fn color_1(&self) -> &Srgb8 {
        &self.colors[0]
    }

    pub fn color_2(&self) -> &Srgb8 {
        &self.colors[1]
    }

    pub fn length(&self) -> usize {
        self.length
    }

    pub fn uuid(&self) -> Uuid {
        self.uuid
    }

    pub fn to_uuid_string(&self) -> String {
        self.uuid.to_string()
    }

    pub fn chill_idx(&self) -> usize {
        self.chill_idx
    }

    pub fn set_chill_idx(&mut self, chill_idx: usize) {
        self.chill_idx = chill_idx;
    }

    pub fn chill_fac(&self) -> u32 {
        self.chill_fac
    }

    pub fn set_chill_fac(&mut self, chill_fac: u32) {
        self.chill_fac = chill_fac;
    }

    pub fn colors_mut(&mut self) -> &mut [Wrap; 2] {
        &mut self.colors
    }

    pub fn set_length(&mut self, length: usize) {
        self.length = length;
    }

    pub fn brightness(&self) -> u8 {
        self.brightness
    }

    pub fn set_brightness(&mut self, brightness: u8) {
        self.brightness = brightness;
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
            self.ms_since_start()
        }

        pub fn set_now(&mut self, now: DateTime<Utc>) {
            self.now = now;
        }

        pub fn ms_since_start(&self) -> u32 {
            let dt = self
                .now
                .signed_duration_since(self.start)
                .to_std()
                .unwrap()
                .as_millis();
            dt as u32 // :&
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
use uuid::Uuid;

#[cfg(feature = "wasm")]
type MAP = IndexMap<String, Segment>;

#[cfg(feature = "esp")]
type MAP = IndexMap<String, Segment>;
// type MAP = IndexMap<String, Segment, std::hash::BuildHasherDefault<hashers::fx_hash::FxHasher>>;

#[derive(PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct State {
    segments: MAP,
}

impl State {
    pub fn new(segments: impl Iterator<Item = Segment>) -> Self {
        Self {
            segments: segments.map(|seg| (seg.uuid().to_string(), seg)).collect(),
        }
    }

    pub fn new_empty() -> Self {
        Self {
            segments: MAP::new(),
        }
    }

    pub fn insert(&mut self, seg: Segment) -> Option<Segment> {
        self.segments.insert(seg.uuid().to_string(), seg)
    }

    pub fn remove(&mut self, segment_id: impl AsRef<str>) -> Option<Segment> {
        self.segments.remove(segment_id.as_ref())
    }

    pub fn segments(&self) -> &MAP {
        &self.segments
    }
}

impl Deref for State {
    type Target = MAP;

    fn deref(&self) -> &Self::Target {
        &self.segments
    }
}

impl DerefMut for State {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.segments
    }
}
