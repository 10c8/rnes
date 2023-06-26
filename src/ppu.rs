use core::panic;
use std::{
    fs::File,
    io::{Read, Write},
};

use crate::cartridge::{Cartridge, MirrorMode};

#[derive(Debug)]
enum IRPart {
    All,
    CoarseX,
    CoarseY,
    Nametable,
    NametableX,
    NametableY,
    FineY,
}

struct InternalRegister {
    value: u16,
}

impl InternalRegister {
    pub fn new() -> Self {
        Self { value: 0x0000 }
    }

    pub fn get(&self, part: IRPart) -> u16 {
        let v = self.value;
        match part {
            IRPart::All => v,
            IRPart::CoarseX => v & 0b0000_0000_0001_1111,
            IRPart::CoarseY => (v & 0b0000_0011_1110_0000) >> 5,
            IRPart::Nametable => (v & 0b0000_1100_0000_0000) >> 10,
            IRPart::NametableX => (v & 0b0000_0100_0000_0000) >> 10,
            IRPart::NametableY => (v & 0b0000_1000_0000_0000) >> 11,
            IRPart::FineY => (v & 0b0111_0000_0000_0000) >> 12,
        }
    }

    pub fn set(&mut self, part: IRPart, value: u16) {
        let v = value;
        match part {
            IRPart::All => self.value = v,
            IRPart::CoarseX => self.value = (self.value & 0b1111_1111_1110_0000) | v,
            IRPart::CoarseY => self.value = (self.value & 0b1111_1100_0001_1111) | (v << 5),
            IRPart::Nametable => self.value = (self.value & 0b1111_0011_1111_1111) | (v << 10),
            IRPart::NametableX => self.value = (self.value & 0b1111_1011_1111_1111) | (v << 10),
            IRPart::NametableY => self.value = (self.value & 0b1111_0111_1111_1111) | (v << 11),
            IRPart::FineY => self.value = (self.value & 0b1000_1111_1111_1111) | (v << 12),
        }
    }

    pub fn set_lo(&mut self, value: u8) {
        self.value = (self.value & 0xFF00) | value as u16;
    }

    pub fn set_hi(&mut self, value: u8) {
        self.value = (self.value & 0x00FF) | (value as u16) << 8;
    }

    pub fn increment_all(&mut self, value: u16) {
        self.value = self.value.wrapping_add(value);

        if self.value > 0x3FFF {
            self.value = (self.value % 0x4000) + 0x2000;
        }
    }

    pub fn increment(&mut self, part: IRPart) {
        match part {
            IRPart::CoarseX => {
                let x = self.get(IRPart::CoarseX);
                self.set(IRPart::CoarseX, x + 1);
            }
            IRPart::CoarseY => {
                let y = self.get(IRPart::CoarseY);
                self.set(IRPart::CoarseY, y + 1);
            }
            IRPart::FineY => {
                let fy = self.get(IRPart::FineY);
                self.set(IRPart::FineY, fy + 1);
            }
            _ => {}
        }
    }

    pub fn switch_horizontal(&mut self) {
        self.value ^= 0b0000_0100_0000_0000;
    }

    pub fn switch_vertical(&mut self) {
        self.value ^= 0b0000_1000_0000_0000;
    }
}

#[derive(Clone, Copy)]
struct Sprite {
    pub x: u8,
    pub y: u8,
    pub tile_id: u8,
    pub attr: u8,
}

impl Sprite {
    pub fn new(x: u8, y: u8, tile_id: u8, attr: u8) -> Self {
        Self {
            x,
            y,
            tile_id,
            attr,
        }
    }
}

impl Default for Sprite {
    fn default() -> Self {
        Self {
            x: 0xFF,
            y: 0xFF,
            tile_id: 0xFF,
            attr: 0xFF,
        }
    }
}

pub struct PPU {
    vram: Vec<u8>,
    name_tables: [[u8; 0x400]; 2],
    palette_table: [u8; 0x20],

    framebuffer: Vec<u8>,
    framebuffer_updated: bool,

    oam_addr: u8,
    oam: Vec<u8>,

    is_odd_frame: bool,

    sprs: [Sprite; 8],
    spr_count: usize,
    spr_shifter_pattern_lo: [u8; 8],
    spr_shifter_pattern_hi: [u8; 8],
    spr_zero_hit_possible: bool,
    spr_zero_rendered: bool,

    system_palette: [usize; 64],

    name_table_addr: u8,
    vram_addr_inc: bool,
    spr_table_addr: bool,
    bg_table_addr: bool,
    spr_size: bool,
    ppu_master_slave: bool,
    nmi_enable: bool,

    monochrome: bool,
    clip_bg: bool,
    clip_spr: bool,
    show_bg: bool,
    show_spr: bool,
    color_emphasis: u8,

    ignore_vram_writes: bool,
    spr_overflow: bool,
    spr_zero_hit: bool,
    vblank: bool,

    v_reg: InternalRegister,
    t_reg: InternalRegister,
    fine_x: u8,

    internal_latch: bool,

    bg_id: u8,
    bg_attr: u8,
    bg_lsb: u8,
    bg_msb: u8,
    bg_shifter_pattern_lo: u16,
    bg_shifter_pattern_hi: u16,
    bg_shifter_attr_lo: u16,
    bg_shifter_attr_hi: u16,

    mirror_mode: MirrorMode,
    data_buffer: u8,

    pub nmi_occurred: bool,
    current_cycle: usize,
    scanline: isize,

    trace_log: File,
}

impl PPU {
    pub fn new() -> Self {
        // Load palette from file
        let mut palette_file = File::open("./palette.pal").unwrap();
        let mut palette_data = vec![0; 64 * 3];
        palette_file
            .read_exact(&mut palette_data)
            .map_err(|e| {
                panic!("Invalid palette file: {}", e);
            })
            .unwrap();

        let mut palette: [usize; 64] = [0; 64];
        for i in 0..palette.len() {
            let r = palette_data[i * 3 + 0] as usize;
            let g = palette_data[i * 3 + 1] as usize;
            let b = palette_data[i * 3 + 2] as usize;

            palette[i] = (r << 16) | (g << 8) | b;
        }

        let trace_log = File::create("./ppu.log").unwrap();

        Self {
            vram: vec![0; 0x10000],
            name_tables: [[0; 0x400]; 2],
            palette_table: [0; 0x20],

            framebuffer: vec![0; 256 * 240 * 4],
            framebuffer_updated: false,

            oam_addr: 0,
            oam: vec![0; 0x100],

            is_odd_frame: false,

            sprs: [Sprite::default(); 8],
            spr_count: 0,
            spr_shifter_pattern_lo: [0; 8],
            spr_shifter_pattern_hi: [0; 8],
            spr_zero_hit_possible: false,
            spr_zero_rendered: false,

            system_palette: palette,

            name_table_addr: 0x00,
            vram_addr_inc: false,
            spr_table_addr: false,
            bg_table_addr: false,
            spr_size: false,
            ppu_master_slave: false,
            nmi_enable: false,

            monochrome: false,
            clip_bg: false,
            clip_spr: false,
            show_bg: false,
            show_spr: false,
            color_emphasis: 0x00,

            ignore_vram_writes: false,
            spr_overflow: false,
            spr_zero_hit: false,
            vblank: false,

            v_reg: InternalRegister::new(),
            t_reg: InternalRegister::new(),
            fine_x: 0,

            internal_latch: false,

            bg_id: 0x00,
            bg_attr: 0x00,
            bg_lsb: 0x00,
            bg_msb: 0x00,
            bg_shifter_pattern_lo: 0x0000,
            bg_shifter_pattern_hi: 0x0000,
            bg_shifter_attr_lo: 0x0000,
            bg_shifter_attr_hi: 0x0000,

            mirror_mode: MirrorMode::Horizontal,

            data_buffer: 0,

            nmi_occurred: false,
            current_cycle: 0,
            scanline: -1,

            trace_log,
        }
    }

    pub fn trace<S: Into<String>>(&mut self, msg: S) {
        let msg = format!(
            "{:03},{:03}  v:{:04X} t:{:04X}  {}\n",
            self.get_scanline(),
            self.current_cycle,
            self.v_reg.get(IRPart::All),
            self.t_reg.get(IRPart::All),
            msg.into()
        );
        self.trace_log.write_all(msg.as_bytes()).unwrap();
    }

    pub fn load_cartridge(&mut self, cartridge: &Cartridge) {
        let vrom = cartridge.mapper.get_vrom().to_vec();
        let bank_count = cartridge.mapper.get_vrom_bank_count();
        self.vram[0x0000..0x2000 * bank_count].copy_from_slice(&vrom);

        self.mirror_mode = cartridge.mirror_mode.clone();
    }

    pub fn cycle(&mut self) {
        if self.scanline >= -1 && self.scanline < 240 {
            if self.is_odd_frame
                && self.scanline == 0
                && self.current_cycle == 0
                && (self.show_bg | self.show_spr)
            {
                self.current_cycle = 1;
            }

            if self.scanline == -1 && self.current_cycle == 1 {
                self.vblank = false;

                self.spr_overflow = false;
                self.spr_zero_hit = false;

                for i in 0..8 {
                    self.spr_shifter_pattern_lo[i] = 0;
                    self.spr_shifter_pattern_hi[i] = 0;
                }
            }

            if (self.current_cycle >= 2 && self.current_cycle < 258)
                || (self.current_cycle >= 321 && self.current_cycle < 338)
            {
                self.update_shifters();

                match (self.current_cycle - 1) & 7 {
                    0 => {
                        self.load_bg_shifters();
                        self.load_bg_id();
                    }
                    2 => {
                        self.load_bg_attr();
                    }
                    4 => {
                        self.load_bg_lsb();
                    }
                    6 => {
                        self.load_bg_msb();
                    }
                    7 => {
                        self.ir_increment_scroll_x();
                    }
                    _ => {}
                }
            }

            if self.current_cycle == 256 {
                self.ir_increment_scroll_y();
            }

            if self.current_cycle == 257 {
                self.load_bg_shifters();
                self.ir_transfer_x();
            }

            if self.scanline == -1 && self.current_cycle >= 280 && self.current_cycle < 305 {
                self.ir_transfer_y();
            }

            if self.current_cycle == 338 || self.current_cycle == 340 {
                self.load_bg_id();
            }

            if self.current_cycle == 257 && (self.show_bg || self.show_spr) {
                self.spr_count = 0;

                for i in 0..8 {
                    self.sprs[i].x = 0xFF;
                    self.sprs[i].y = 0xFF;

                    self.spr_shifter_pattern_lo[i] = 0;
                    self.spr_shifter_pattern_hi[i] = 0;
                }

                if self.scanline >= 0 && self.scanline < 240 {
                    self.evaluate_sprs();
                }
            }

            if self.current_cycle == 340 {
                self.load_spr_shifters();
            }
        }

        // Render
        if self.current_cycle > 0
            && self.current_cycle <= 256
            && self.scanline >= 0
            && self.scanline < 240
        {
            let mut bg_pixel = 0x00;
            let mut bg_palette = 0x00;

            let mut spr_pixel = 0x00;
            let mut spr_palette = 0x00;
            let mut spr_priority = false;

            if self.show_bg && (!self.clip_bg || self.current_cycle > 8) {
                let mux = 0x8000 >> self.fine_x;

                let pixel_lsb = (self.bg_shifter_pattern_lo & mux) != 0;
                let pixel_msb = (self.bg_shifter_pattern_hi & mux) != 0;
                bg_pixel = ((pixel_msb as u8) << 1) | pixel_lsb as u8;

                let palette_lsb = (self.bg_shifter_attr_lo & mux) != 0;
                let palette_msb = (self.bg_shifter_attr_hi & mux) != 0;
                bg_palette = ((palette_msb as u8) << 1) | palette_lsb as u8;
            }

            if self.show_spr && (!self.clip_spr || self.current_cycle > 8) {
                self.spr_zero_rendered = false;

                for i in 0..self.spr_count {
                    let spr = self.sprs[i].clone();
                    let offset = (self.current_cycle - 1) as i16 - spr.x as i16;

                    if offset >= 0 && offset < 8 {
                        let mux = 0x80 >> offset;

                        let pixel_lsb = (self.spr_shifter_pattern_lo[i] & mux) != 0;
                        let pixel_msb = (self.spr_shifter_pattern_hi[i] & mux) != 0;
                        spr_pixel = ((pixel_msb as u8) << 1) | pixel_lsb as u8;

                        spr_palette = (spr.attr as u8 & 0x03) + 0x04;
                        spr_priority = (spr.attr & 0x20) == 0;

                        if spr_pixel != 0 {
                            if i == 0 {
                                self.spr_zero_rendered = true;
                            }
                        }
                    }
                }
            }

            let (pixel, palette) = match (bg_pixel, spr_pixel) {
                (0, _) => (spr_pixel, spr_palette),
                (_, 0) => (bg_pixel, bg_palette),
                (_, _) => {
                    if spr_priority {
                        (spr_pixel, spr_palette)
                    } else {
                        (bg_pixel, bg_palette)
                    }
                }
            };

            if (self.spr_zero_hit_possible && self.spr_zero_rendered)
                && (self.current_cycle > 0 && self.current_cycle < 256)
            {
                self.spr_zero_hit = true;
            }

            let color = self.get_color(palette, pixel);
            self.set_framebuffer_pixel(self.current_cycle - 1, self.scanline as usize, color);
        }

        if self.scanline == 241 && self.current_cycle == 1 {
            self.vblank = true;

            if self.nmi_enable {
                self.nmi_occurred = true;
            }
        }

        self.current_cycle += 1;

        if self.current_cycle >= 341 {
            self.current_cycle = 0;
            self.scanline += 1;

            if self.scanline >= 261 {
                self.scanline = -1;
                self.framebuffer_updated = true;
                self.is_odd_frame = !self.is_odd_frame;
            }
        }
    }

    pub fn get_scanline(&self) -> usize {
        self.scanline.wrapping_add(1) as usize
    }

    pub fn get_current_cycle(&self) -> usize {
        self.current_cycle
    }

    pub fn framebuffer_has_changed(&mut self) -> bool {
        let changed = self.framebuffer_updated;
        if changed {
            self.framebuffer_updated = false;
        }

        changed
    }

    pub fn get_framebuffer(&self) -> &[u8] {
        &self.framebuffer
    }

    pub fn get_system_palette(&self) -> &[usize] {
        &self.system_palette
    }

    pub fn get_image_palette(&self) -> Vec<u8> {
        self.palette_table[0..16].to_vec()
    }

    pub fn get_sprite_palette(&self) -> Vec<u8> {
        self.palette_table[16..32].to_vec()
    }

    pub fn set_control1(&mut self, value: u8) {
        // $2000 write
        self.name_table_addr = value & 0b0000_0011;
        self.vram_addr_inc = value & 0b0000_0100 != 0;
        self.spr_table_addr = value & 0b0000_1000 != 0;
        self.bg_table_addr = value & 0b0001_0000 != 0;
        self.spr_size = value & 0b0010_0000 != 0;
        self.ppu_master_slave = value & 0b0100_0000 != 0;
        self.nmi_enable = value & 0b1000_0000 != 0;

        self.t_reg
            .set(IRPart::Nametable, (value as u16) & 0b0000_0011);
    }

    pub fn set_control2(&mut self, value: u8) {
        // $2001 write
        self.monochrome = value & 0b0000_0001 != 0;
        self.clip_bg = value & 0b0000_0010 != 0;
        self.clip_spr = value & 0b0000_0100 != 0;
        self.show_bg = value & 0b0000_1000 != 0;
        self.show_spr = value & 0b0001_0000 != 0;
        self.color_emphasis = value & 0b1110_0000;
    }

    pub fn read_status(&mut self) -> u8 {
        // $2002 read
        let mut status = 0x00;

        if self.ignore_vram_writes {
            status |= 0b0001_0000;
        }

        if self.spr_overflow {
            status |= 0b0010_0000;
        }

        if self.spr_zero_hit {
            status |= 0b0100_0000;
        }

        if self.vblank {
            status |= 0b1000_0000;
            self.vblank = false;
        }

        self.internal_latch = false;

        status | (self.data_buffer & 0b0001_1111)
    }

    pub fn set_oam_address(&mut self, value: u8) {
        // $2003 write
        self.oam_addr = value;
    }

    pub fn set_scroll(&mut self, value: u8) {
        // $2005 write
        let value = value as u16;

        if !self.internal_latch {
            self.t_reg.set(IRPart::CoarseX, value >> 3);
            self.fine_x = (value as u8) & 0b0000_0111;
        } else {
            self.t_reg.set(IRPart::CoarseY, value >> 3);
            self.t_reg.set(IRPart::FineY, value & 0b0000_0111);
        }

        self.internal_latch = !self.internal_latch;
    }

    pub fn set_vram_address(&mut self, value: u8) {
        // $2006 write
        if !self.internal_latch {
            self.t_reg.set_hi(value);
        } else {
            self.t_reg.set_lo(value);
            self.v_reg.set(IRPart::All, self.t_reg.get(IRPart::All));
        }

        self.internal_latch = !self.internal_latch;
    }

    pub fn vram_read(&mut self) -> u8 {
        // $2007 read
        let mut result = self.data_buffer;

        let v = self.v_reg.get(IRPart::All);
        self.data_buffer = self.internal_read(v);

        if v & 0x3FFF >= 0x3F00 {
            result = self.data_buffer;
            self.data_buffer = self.internal_read(v.wrapping_sub(0x1000));
        }

        let inc = if self.vram_addr_inc { 32 } else { 1 };
        self.v_reg.increment_all(inc);

        result
    }

    pub fn vram_write(&mut self, data: u8) {
        // $2007 write
        if self.ignore_vram_writes {
            return;
        }

        let v = self.v_reg.get(IRPart::All);
        self.internal_write(v, data);

        let inc = if self.vram_addr_inc { 32 } else { 1 };
        self.v_reg.increment_all(inc);
    }

    pub fn oam_read(&self) -> u8 {
        self.oam[self.oam_addr as usize]
    }

    pub fn oam_write(&mut self, data: u8) {
        self.oam[self.oam_addr as usize] = data;
        self.oam_addr = self.oam_addr.wrapping_add(1);
    }

    fn internal_read(&mut self, address: u16) -> u8 {
        let address = address as usize;

        match address {
            0x3F00..=0x3FFF => {
                let address = address & 0x001F;
                let address = if address > 0x0020 {
                    address - 0x0020
                } else {
                    address
                };

                let data = self.palette_table[address];

                if self.monochrome {
                    data & 0x30
                } else {
                    data & 0x3F
                }
            }
            0x2000..=0x3EFF => {
                let address = address & 0x0FFF;

                match self.mirror_mode {
                    MirrorMode::Vertical => match address {
                        0x0000..=0x03FF => self.name_tables[0][address & 0x03FF],
                        0x0400..=0x07FF => self.name_tables[1][address & 0x03FF],
                        0x0800..=0x0BFF => self.name_tables[0][address & 0x03FF],
                        0x0C00..=0x0FFF => self.name_tables[1][address & 0x03FF],
                        _ => unreachable!(),
                    },
                    MirrorMode::Horizontal => match address {
                        0x0000..=0x03FF => self.name_tables[0][address & 0x03FF],
                        0x0400..=0x07FF => self.name_tables[0][address & 0x03FF],
                        0x0800..=0x0BFF => self.name_tables[1][address & 0x03FF],
                        0x0C00..=0x0FFF => self.name_tables[1][address & 0x03FF],
                        _ => unreachable!(),
                    },

                    _ => panic!("Unsupported mirroring mode: {:?}", self.mirror_mode),
                }
            }
            _ => self.vram[address as usize],
        }
    }

    fn internal_write(&mut self, address: u16, value: u8) {
        let address = address as usize;

        match address {
            0x2000..=0x3EFF => {
                let address = address & 0x0FFF;

                match self.mirror_mode {
                    MirrorMode::Vertical => match address {
                        0x0000..=0x03FF => self.name_tables[0][address & 0x03FF] = value,
                        0x0400..=0x07FF => self.name_tables[1][address & 0x03FF] = value,
                        0x0800..=0x0BFF => self.name_tables[0][address & 0x03FF] = value,
                        0x0C00..=0x0FFF => self.name_tables[1][address & 0x03FF] = value,
                        _ => unreachable!(),
                    },
                    MirrorMode::Horizontal => match address {
                        0x0000..=0x03FF => self.name_tables[0][address & 0x03FF] = value,
                        0x0400..=0x07FF => self.name_tables[0][address & 0x03FF] = value,
                        0x0800..=0x0BFF => self.name_tables[1][address & 0x03FF] = value,
                        0x0C00..=0x0FFF => self.name_tables[1][address & 0x03FF] = value,
                        _ => unreachable!(),
                    },

                    _ => panic!("Unsupported mirroring mode: {:?}", self.mirror_mode),
                }
            }
            0x3F00..=0x3FFF => {
                let address = address & 0x001F;
                let address = if address > 0x0020 {
                    address - 0x0020
                } else {
                    address
                };

                self.palette_table[address] = value;
            }
            _ => {}
        }
    }

    fn get_color(&mut self, palette: u8, index: u8) -> usize {
        let color_addr = 0x3F00 + ((palette as u16) << 2) + index as u16;
        let color = self.internal_read(color_addr as u16) & 0x3F;
        self.system_palette[color as usize]
    }

    fn set_framebuffer_pixel(&mut self, x: usize, y: usize, color: usize) {
        let offset = (y * 256 + x) * 4;
        self.framebuffer[offset] = ((color >> 16) & 0xFF) as u8;
        self.framebuffer[offset + 1] = ((color >> 8) & 0xFF) as u8;
        self.framebuffer[offset + 2] = (color & 0xFF) as u8;
        self.framebuffer[offset + 3] = 0xFF;
    }

    fn ir_increment_scroll_x(&mut self) {
        if !self.show_bg && !self.show_spr {
            return;
        }

        let coarse_x = self.v_reg.get(IRPart::CoarseX);
        if coarse_x == 31 {
            self.v_reg.set(IRPart::CoarseX, 0);
            self.v_reg.switch_horizontal();
        } else {
            self.v_reg.increment(IRPart::CoarseX);
        }
    }

    fn ir_increment_scroll_y(&mut self) {
        if !self.show_bg && !self.show_spr {
            return;
        }

        let fine_y = self.v_reg.get(IRPart::FineY);
        if fine_y < 7 {
            self.v_reg.increment(IRPart::FineY);
        } else {
            self.v_reg.set(IRPart::FineY, 0);

            let coarse_y = self.v_reg.get(IRPart::CoarseY);
            if coarse_y == 29 {
                self.v_reg.set(IRPart::CoarseY, 0);
                self.v_reg.switch_vertical();
            } else if coarse_y == 31 {
                self.v_reg.set(IRPart::CoarseY, 0);
            } else {
                self.v_reg.increment(IRPart::CoarseY);
            }
        }
    }

    fn ir_transfer_x(&mut self) {
        if !self.show_bg && !self.show_spr {
            return;
        }

        let coarse_x = self.t_reg.get(IRPart::CoarseX);
        self.v_reg.set(IRPart::CoarseX, coarse_x);

        let nametable_x = self.t_reg.get(IRPart::NametableX);
        self.v_reg.set(IRPart::NametableX, nametable_x);
    }

    fn ir_transfer_y(&mut self) {
        if !self.show_bg && !self.show_spr {
            return;
        }

        let coarse_y = self.t_reg.get(IRPart::CoarseY);
        self.v_reg.set(IRPart::CoarseY, coarse_y);

        let nametable_y = self.t_reg.get(IRPart::NametableY);
        self.v_reg.set(IRPart::NametableY, nametable_y);

        let fine_y = self.t_reg.get(IRPart::FineY);
        self.v_reg.set(IRPart::FineY, fine_y);
    }

    fn load_bg_id(&mut self) {
        let v = self.v_reg.get(IRPart::All);
        let address = 0x2000 | (v & 0x0FFF);
        self.bg_id = self.internal_read(address);
    }

    fn load_bg_attr(&mut self) {
        let address = 0x23C0
            | (self.v_reg.get(IRPart::NametableY) << 11)
            | (self.v_reg.get(IRPart::NametableX) << 10)
            | ((self.v_reg.get(IRPart::CoarseY) >> 2) << 3)
            | (self.v_reg.get(IRPart::CoarseX) >> 2);
        self.bg_attr = self.internal_read(address);

        if self.v_reg.get(IRPart::CoarseX) & 2 != 0 {
            self.bg_attr = self.bg_attr.wrapping_shr(2);
        }

        if self.v_reg.get(IRPart::CoarseY) & 2 != 0 {
            self.bg_attr = self.bg_attr.wrapping_shr(4);
        }

        self.bg_attr &= 0x03;
    }

    fn load_bg_lsb(&mut self) {
        let address = ((self.bg_table_addr as u16) << 12)
            + ((self.bg_id as u16) << 4)
            + self.v_reg.get(IRPart::FineY);
        self.bg_lsb = self.internal_read(address);
    }

    fn load_bg_msb(&mut self) {
        let address = ((self.bg_table_addr as u16) << 12)
            + ((self.bg_id as u16) << 4)
            + self.v_reg.get(IRPart::FineY)
            + 8;
        self.bg_msb = self.internal_read(address);
    }

    fn load_bg_shifters(&mut self) {
        self.bg_shifter_pattern_lo = (self.bg_shifter_pattern_lo & 0xFF00) | (self.bg_lsb as u16);
        self.bg_shifter_pattern_hi = (self.bg_shifter_pattern_hi & 0xFF00) | (self.bg_msb as u16);

        let color = if self.bg_attr & 0x01 != 0 { 0xFF } else { 0 };
        self.bg_shifter_attr_lo = (self.bg_shifter_attr_lo & 0xFF00) | color;

        let color = if self.bg_attr & 0x02 != 0 { 0xFF } else { 0 };
        self.bg_shifter_attr_hi = (self.bg_shifter_attr_hi & 0xFF00) | color;
    }

    fn load_spr_shifters(&mut self) {
        let mut spr_pattern_bits_lo;
        let mut spr_pattern_bits_hi;

        let mut spr_pattern_addr_lo;
        let mut spr_pattern_addr_hi;

        for i in 0..self.spr_count {
            let spr = self.sprs[i].clone();
            let offset = (self.scanline as u16) - (spr.y as u16);

            if !self.spr_size {
                // 8x8 sprites
                if spr.attr & 0x80 == 0 {
                    // No horizontal flip
                    spr_pattern_addr_lo =
                        ((self.spr_table_addr as u16) << 12) | ((spr.tile_id as u16) << 4) | offset;
                } else {
                    // Horizontal flip
                    spr_pattern_addr_lo = ((self.spr_table_addr as u16) << 12)
                        | ((spr.tile_id as u16) << 4)
                        | (7 - offset);
                }
            } else {
                // 8x16 sprites
                if spr.attr & 0x80 == 0 {
                    // No horizontal flip
                    if offset < 8 {
                        // Top half
                        spr_pattern_addr_lo = (((spr.tile_id as u16) & 0x01) << 12)
                            | (((spr.tile_id as u16) & 0xFE) << 4)
                            | (offset & 0x07);
                    } else {
                        // Bottom half
                        spr_pattern_addr_lo = (((spr.tile_id as u16) & 0x01) << 12)
                            | ((((spr.tile_id as u16) & 0xFE) + 1) << 4)
                            | (offset & 0x07);
                    }
                } else {
                    // Horizontal flip
                    if offset < 8 {
                        // Top half
                        spr_pattern_addr_lo = (((spr.tile_id as u16) & 0x01) << 12)
                            | ((((spr.tile_id as u16) & 0xFE) + 1) << 4)
                            | (7 - (offset & 0x07));
                    } else {
                        // Bottom half
                        spr_pattern_addr_lo = (((spr.tile_id as u16) & 0x01) << 12)
                            | (((spr.tile_id as u16) & 0xFE) << 4)
                            | (7 - (offset & 0x07));
                    }
                }
            }

            spr_pattern_addr_hi = spr_pattern_addr_lo.wrapping_add(8);

            spr_pattern_bits_lo = self.internal_read(spr_pattern_addr_lo);
            spr_pattern_bits_hi = self.internal_read(spr_pattern_addr_hi);

            if spr.attr & 0x40 != 0 {
                // Vertical flip
                spr_pattern_bits_lo = reverse_bits(spr_pattern_bits_lo);
                spr_pattern_bits_hi = reverse_bits(spr_pattern_bits_hi);
            }

            self.spr_shifter_pattern_lo[i] = spr_pattern_bits_lo;
            self.spr_shifter_pattern_hi[i] = spr_pattern_bits_hi;
        }
    }

    fn update_shifters(&mut self) {
        if self.show_bg {
            self.bg_shifter_pattern_lo = self.bg_shifter_pattern_lo.wrapping_shl(1);
            self.bg_shifter_pattern_hi = self.bg_shifter_pattern_hi.wrapping_shl(1);

            self.bg_shifter_attr_lo = self.bg_shifter_attr_lo.wrapping_shl(1);
            self.bg_shifter_attr_hi = self.bg_shifter_attr_hi.wrapping_shl(1);
        }

        if self.show_spr && self.current_cycle >= 1 && self.current_cycle < 258 {
            for i in 0..self.spr_count {
                if self.sprs[i].x > 0 {
                    self.sprs[i].x = self.sprs[i].x.wrapping_sub(1);
                } else {
                    self.spr_shifter_pattern_lo[i] = self.spr_shifter_pattern_lo[i].wrapping_shl(1);
                    self.spr_shifter_pattern_hi[i] = self.spr_shifter_pattern_hi[i].wrapping_shl(1);
                }
            }
        }
    }

    fn evaluate_sprs(&mut self) {
        self.spr_zero_hit_possible = false;

        for i in (0..256).step_by(4) {
            if self.spr_count > 8 {
                break;
            }

            let spr = Sprite::new(
                self.oam[i],
                self.oam[i + 1],
                self.oam[i + 2],
                self.oam[i + 3],
            );

            let diff = self.scanline as i16 - spr.y as i16;
            let spr_size = if self.spr_size { 16 } else { 8 };

            if diff >= 0 && diff < spr_size {
                if self.spr_count < 8 {
                    if i == 0 {
                        self.spr_zero_hit_possible = true;
                    }

                    self.sprs[self.spr_count] = spr;
                    self.spr_count += 1;
                } else {
                    self.spr_overflow = true;
                }
            }
        }
    }
}

// Helpers
fn reverse_bits(n: u8) -> u8 {
    let mut n = n;
    n = (n & 0xF0) >> 4 | (n & 0x0F) << 4;
    n = (n & 0xCC) >> 2 | (n & 0x33) << 2;
    n = (n & 0xAA) >> 1 | (n & 0x55) << 1;
    n
}
