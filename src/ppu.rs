use core::panic;
use std::{fs::File, io::Read};

use crate::cartridge::Cartridge;

enum SpritePriority {
    Front,
    Back,
}

pub struct PPU {
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
    hide_bg: bool,
    hide_spr: bool,
    color_emphasis: u8,

    ignore_vram_writes: bool,
    scanline_spr_count: bool,
    spr0_hit: bool,
    vblank: bool,

    addr_latch: bool,

    #[allow(dead_code)]
    read_buffer: u8,

    io_bus: u8,

    oam_addr: u8,
    oam: Vec<u8>,

    vram_addr: u16,
    vram: Vec<u8>,

    framebuffer: Vec<u8>,
    framebuffer_updated: bool,

    pub nmi_occurred: bool,
    current_cycle: usize,
    scanline: usize,
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

        Self {
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
            hide_bg: false,
            hide_spr: false,
            color_emphasis: 0x00,

            ignore_vram_writes: false,
            scanline_spr_count: false,
            spr0_hit: false,
            vblank: false,

            addr_latch: false,

            read_buffer: 0,

            io_bus: 0,

            oam_addr: 0,
            oam: vec![0; 0x100],

            vram_addr: 0x0000,
            vram: vec![0; 0x10000],

            framebuffer: vec![0; 256 * 240 * 4],
            framebuffer_updated: false,

            nmi_occurred: false,
            current_cycle: 0,
            scanline: 0,
        }
    }

    pub fn load_cartridge(&mut self, cartridge: &Cartridge) {
        let vrom = cartridge.mapper.get_vrom().to_vec();
        let bank_count = cartridge.mapper.get_vrom_bank_count();
        self.vram[0x0000..0x2000 * bank_count].copy_from_slice(&vrom);
    }

    pub fn cycle(&mut self) {
        self.current_cycle += 1;

        if self.current_cycle == 341 {
            self.current_cycle = 0;
            self.scanline += 1;

            if self.scanline < 240 {
                self.draw_sprite_scanline(SpritePriority::Back);
                self.draw_bg_scanline();
                self.draw_sprite_scanline(SpritePriority::Front);

                self.framebuffer_updated = true;
            } else if self.scanline == 241 {
                // Trigger vblank
                self.vblank = true;

                if self.nmi_enable {
                    self.nmi_occurred = true;
                }
            } else if self.scanline == 262 {
                // End vblank
                self.vblank = false;
                self.nmi_occurred = false;
                self.scanline = 0;
            }
        }
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
        self.vram[0x3F00..0x3F10].to_vec()
    }

    pub fn get_sprite_palette(&self) -> Vec<u8> {
        self.vram[0x3F10..0x3F20].to_vec()
    }

    pub fn read_name_table_address(&self) -> u16 {
        match self.name_table_addr {
            0x00 => 0x2000,
            0x01 => 0x2400,
            0x02 => 0x2800,
            0x03 => 0x2C00,
            _ => panic!("Invalid nametable address: {:#X}", self.name_table_addr),
        }
    }

    pub fn read_bg_table_address(&self) -> u16 {
        if self.bg_table_addr {
            0x1000
        } else {
            0x0000
        }
    }

    pub fn read_spr_table_address(&self) -> u16 {
        if self.spr_table_addr {
            0x1000
        } else {
            0x0000
        }
    }

    pub fn read_status(&mut self) -> u8 {
        let mut status = 0x00;

        if self.ignore_vram_writes {
            status |= 0b0001_0000;
        }

        if self.scanline_spr_count {
            status |= 0b0010_0000;
        }

        if self.spr0_hit {
            status |= 0b0100_0000;
        }

        if self.vblank {
            status |= 0b1000_0000;
        }

        self.vblank = false;

        status | (self.io_bus & 0b0001_1111)
    }

    pub fn set_control1(&mut self, value: u8) {
        self.name_table_addr = value & 0b0000_0011;
        self.vram_addr_inc = value & 0b0000_0100 != 0;
        self.spr_table_addr = value & 0b0000_1000 != 0;
        self.bg_table_addr = value & 0b0001_0000 != 0;
        self.spr_size = value & 0b0010_0000 != 0;
        self.ppu_master_slave = value & 0b0100_0000 != 0;
        self.nmi_enable = value & 0b1000_0000 != 0;

        self.io_bus = (self.io_bus & 0b1110_0000) | (value & 0b0001_1111);
    }

    pub fn set_control2(&mut self, value: u8) {
        self.monochrome = value & 0b0000_0001 != 0;
        self.clip_bg = value & 0b0000_0010 != 0;
        self.clip_spr = value & 0b0000_0100 != 0;
        self.hide_bg = value & 0b0000_1000 != 0;
        self.hide_spr = value & 0b0001_0000 != 0;
        self.color_emphasis = value & 0b1110_0000;

        self.io_bus = (self.io_bus & 0b0001_1111) | (value & 0b1110_0000);
    }

    pub fn set_scroll(&mut self, value: u8) {
        self.io_bus = (self.io_bus & 0b1110_0000) | (value & 0b0001_1111);
    }

    pub fn set_vram_address(&mut self, value: u8) {
        if self.addr_latch {
            self.vram_addr = (self.vram_addr & 0xFF00) | value as u16;
        } else {
            self.vram_addr = (self.vram_addr & 0x00FF) | ((value as u16) << 8);
        }

        self.addr_latch = !self.addr_latch;
        self.io_bus = (self.io_bus & 0b1110_0000) | (value & 0b0001_1111);
    }

    pub fn set_oam_address(&mut self, value: u8) {
        self.oam_addr = value;
        self.io_bus = (self.io_bus & 0b1110_0000) | (value & 0b0001_1111);
    }

    pub fn vram_write(&mut self, data: u8) {
        if self.ignore_vram_writes {
            return;
        }

        let vram_inc = if self.vram_addr_inc { 32 } else { 1 };

        self.vram[self.vram_addr as usize] = data;
        self.vram_addr = self.vram_addr.wrapping_add(vram_inc);
    }

    pub fn oam_write(&mut self, data: u8) {
        self.oam[self.oam_addr as usize] = data;
        self.oam_addr = self.oam_addr.wrapping_add(1);
    }

    fn set_framebuffer_pixel(&mut self, x: usize, y: usize, color: usize) {
        let offset = (y * 256 + x) * 4;
        self.framebuffer[offset] = ((color >> 16) & 0xFF) as u8;
        self.framebuffer[offset + 1] = ((color >> 8) & 0xFF) as u8;
        self.framebuffer[offset + 2] = (color & 0xFF) as u8;
        self.framebuffer[offset + 3] = 0xFF;
    }

    fn draw_bg_scanline(&mut self) {
        let row = self.scanline / 8;

        let table = self.read_name_table_address();
        let bank = self.read_bg_table_address();
        let image_palette = self.get_image_palette();

        for col in 0..32 {
            let tile_addr = table as usize + (row * 32 + col);
            let pattern = bank | ((self.vram[tile_addr] as u16) << 4);

            let attr_addr = table as usize + 0x03C0 + (row / 4) * 8 + (col / 4);
            let attr = self.vram[attr_addr as usize];
            let palette_id = (attr >> (2 * (col % 4 / 2) + 4 * (row % 4 / 2))) & 0x03;

            let y = self.scanline % 8;
            for x in 0..8 {
                let color_idx_lo = self.vram[(pattern + y as u16) as usize];
                let color_idx_lo = (color_idx_lo >> (7 - x)) & 1;
                let color_idx_hi = self.vram[(pattern + y as u16 + 8) as usize];
                let color_idx_hi = (color_idx_hi >> (7 - x)) & 1;
                let color_idx = (color_idx_hi << 1 | color_idx_lo) as usize;

                let color_id = image_palette[palette_id as usize * 4 + color_idx];
                let color_hex = self.system_palette[color_id as usize];

                self.set_framebuffer_pixel(col * 8 + x, row * 8 + y, color_hex);
            }
        }
    }

    fn draw_sprite_scanline(&mut self, priority_type: SpritePriority) {
        if self.scanline == 0 {
            return;
        }

        let table = self.read_spr_table_address();
        let sprite_palette = self.get_sprite_palette();

        for sprite in (0..256).step_by(4).rev() {
            let attr = self.oam[sprite + 2];
            let priority = attr & 0b00100000 != 0;

            match priority_type {
                SpritePriority::Front if priority => {
                    continue;
                }
                SpritePriority::Back if !priority => {
                    continue;
                }
                _ => {}
            }

            let sprite_x = self.oam[sprite + 3] as usize;
            let sprite_y = self.oam[sprite] as usize;

            if sprite_y > self.scanline || sprite_y + 8 <= self.scanline || sprite_y >= 0xEF {
                continue;
            }

            let pattern_id = self.oam[sprite + 1];
            let pattern = table as usize + (pattern_id as usize) * 16;

            let palette_id = attr & 0b00000011;
            let flip_h = (attr & 0b01000000) != 0;
            let flip_v = (attr & 0b10000000) != 0;

            let y = self.scanline - sprite_y;
            for x in 0..8 {
                let color_idx_lo = self.vram[(pattern + y) as usize];
                let color_idx_lo = (color_idx_lo >> (7 - x)) & 1;
                let color_idx_hi = self.vram[(pattern + y + 8) as usize];
                let color_idx_hi = (color_idx_hi >> (7 - x)) & 1;
                let color_idx = (color_idx_hi << 1 | color_idx_lo) as usize;

                if color_idx != 0 {
                    let color_id = sprite_palette[palette_id as usize * 4 + color_idx];
                    let color_hex = self.system_palette[color_id as usize];

                    let x = if flip_h { 7 - x } else { x };
                    let y = if flip_v { 7 - y } else { y };
                    self.set_framebuffer_pixel(sprite_x + x, sprite_y + y, color_hex);
                }
            }
        }
    }
}
