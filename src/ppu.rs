use crate::cartridge::Cartridge;

// pub const PALETTE: [usize; 64] = [
//     0x7C7C7C, 0x0000FC, 0x0000BC, 0x4428BC, 0x940084, 0xA80020, 0xA81000, 0x881400, 0x503000,
//     0x007800, 0x006800, 0x005800, 0x004058, 0x000000, 0x000000, 0x000000, 0xBCBCBC, 0x0078F8,
//     0x0058F8, 0x6844FC, 0xD800CC, 0xE40058, 0xF83800, 0xE45C10, 0xAC7C00, 0x00B800, 0x00A800,
//     0x00A844, 0x008888, 0x000000, 0x000000, 0x000000, 0xF8F8F8, 0x3CBCFC, 0x6888FC, 0x9878F8,
//     0xF878F8, 0xF85898, 0xF87858, 0xFCA044, 0xF8B800, 0xB8F818, 0x58D854, 0x58F898, 0x00E8D8,
//     0x787878, 0x000000, 0x000000, 0xFCFCFC, 0xA4E4FC, 0xB8B8F8, 0xD8B8F8, 0xF8B8F8, 0xF8A4C0,
//     0xF0D0B0, 0xFCE0A8, 0xF8D878, 0xD8F878, 0xB8F8B8, 0xB8F8D8, 0x00FCFC, 0xF8D8F8, 0x000000,
//     0x000000,
// ];

pub const PALETTE: [usize; 64] = [
    0x788084, 0x0000FC, 0x0000C4, 0x9C78FC, 0x94008C, 0xAC0028, 0xAC1000, 0x8C1800, 0x503C00,
    0x007800, 0x006800, 0x005800, 0x004058, 0x000000, 0x000000, 0x000000, 0xBCC0C4, 0x0078FC,
    0x0058FC, 0x6844FC, 0xD800CC, 0xE40058, 0xF83800, 0xE45C10, 0xAC7C00, 0x00B800, 0x00A800,
    0x00A844, 0x008888, 0x000000, 0x000000, 0x000000, 0xFCFCFC, 0x3CBCFC, 0x6888FC, 0x9878F8,
    0xF878F8, 0xF85898, 0xF87858, 0xFCA044, 0xF8B800, 0xB8F818, 0x58D854, 0x58F898, 0x00E8D8,
    0x787878, 0x000000, 0x000000, 0xFCFCFC, 0xA4E4FC, 0xB8B8F8, 0xD8B8F8, 0xF8B8F8, 0xF8A4C0,
    0xF0D0B0, 0xFCE0A8, 0xF8D878, 0xD8F878, 0xB8F8B8, 0xB8F8D8, 0x00FCFC, 0xF8D8F8, 0x000000,
    0x000000,
];

pub struct PPU {
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
        Self {
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
                // Draw scanline
                let row = self.scanline / 8;
                let table = self.read_name_table_address();
                let bank = self.read_bg_table_address();

                let image_palette = self.get_image_palette();
                let sprite_palette = self.get_sprite_palette();

                // Background
                for col in 0..32 {
                    let tile_addr = table as usize + (row * 32 + col);
                    let pattern_id = self.vram[tile_addr];

                    let attr_addr = table as usize + 0x03C0 + (row / 4) * 8 + (col / 4);
                    let attr = self.vram[attr_addr as usize];

                    let tile_id = ((row / 8) * 32) + (col / 8);
                    let attr_offset = (((tile_id % 32) / 2 % 2) + (tile_id / 64 % 2) * 2) * 2;
                    let palette_id = (attr >> attr_offset) & 3;

                    let pattern_addr = bank | ((pattern_id as u16) << 4);

                    let y = self.scanline % 8;
                    for x in 0..8 {
                        let color_idx_lo = self.vram[(pattern_addr + y as u16) as usize];
                        let color_idx_lo = (color_idx_lo >> (7 - x)) & 1;
                        let color_idx_hi = self.vram[(pattern_addr + y as u16 + 8) as usize];
                        let color_idx_hi = (color_idx_hi >> (7 - x)) & 1;
                        let color_idx = (color_idx_hi << 1 | color_idx_lo) as usize;

                        let color_id = image_palette[palette_id as usize * 4 + color_idx];
                        let color_hex = PALETTE[color_id as usize];

                        let pixel = ((row * 8 + y) * 256 + (col * 8 + x)) as usize;

                        self.framebuffer[pixel * 4] = ((color_hex >> 16) & 0xFF) as u8;
                        self.framebuffer[pixel * 4 + 1] = ((color_hex >> 8) & 0xFF) as u8;
                        self.framebuffer[pixel * 4 + 2] = (color_hex & 0xFF) as u8;
                        self.framebuffer[pixel * 4 + 3] = 255;
                    }
                }

                // TODO: Draw sprites

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

    pub fn get_current_cycle(&self) -> usize {
        self.current_cycle
    }

    pub fn get_scanline(&self) -> usize {
        self.scanline
    }

    pub fn get_framebuffer(&self) -> &[u8] {
        &self.framebuffer
    }

    pub fn framebuffer_has_changed(&mut self) -> bool {
        let changed = self.framebuffer_updated;
        if changed {
            self.framebuffer_updated = false;
        }

        changed
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

    pub fn vram_read(&mut self) -> u8 {
        let result = self.read_buffer;

        let vram_inc = if self.vram_addr_inc { 32 } else { 1 };

        self.read_buffer = self.vram[self.vram_addr as usize];
        self.vram_addr = self.vram_addr.wrapping_add(vram_inc);

        result
    }

    pub fn vram_write(&mut self, data: u8) {
        if self.ignore_vram_writes {
            return;
        }

        let vram_inc = if self.vram_addr_inc { 32 } else { 1 };

        self.vram[self.vram_addr as usize] = data;
        self.vram_addr = self.vram_addr.wrapping_add(vram_inc);
    }

    pub fn oam_read(&self) -> u8 {
        self.oam[self.oam_addr as usize]
    }

    pub fn oam_write(&mut self, data: u8) {
        self.oam[self.oam_addr as usize] = data;
    }
}
