mod cartridge;
mod cpu;
mod mapper;
mod memory;
mod ppu;

use log::info;
use notan::{
    draw::{CreateDraw, DrawConfig, DrawShapes},
    egui::{self, *},
    prelude::*,
};

use cartridge::Cartridge;
use cpu::CPU;

#[derive(AppState)]
struct State {
    cpu: CPU,

    screen_tex: Texture,
    screen_tex_id: TextureId,
}

#[notan_main]
fn main() -> Result<(), String> {
    let win = WindowConfig::new().title("rnes").size(640, 480).vsync(true);

    notan::init_with(init)
        .add_config(win)
        .add_config(DrawConfig)
        .add_config(EguiConfig)
        .update(update)
        .draw(draw)
        .build()
}

fn init(_app: &mut App, gfx: &mut Graphics) -> State {
    info!("Initializing CPU...");
    let mut cpu = CPU::new();

    // info!("Running Tom Harte's CPU tests...");
    // cpu.run_tomharte_tests();

    info!("Loading cartridge from ROM file...");
    let cart = Cartridge::from_rom_file("resources/roms/games/Donkey Kong (U) (PRG1) [!].nes");
    // let cart = Cartridge::from_rom_file("resources/roms/nestest.nes");
    cpu.load_cartridge(cart);

    info!("Starting...");
    cpu.reset();

    let screen_tex = gfx
        .create_texture()
        .from_bytes(&cpu.get_ppu_framebuffer(), 256, 240)
        .build()
        .unwrap();

    let screen_tex_id = gfx.egui_register_texture(&screen_tex);

    State {
        cpu,
        screen_tex,
        screen_tex_id,
    }
}

fn update(app: &mut App, state: &mut State) {
    let cycles = state.cpu.get_cycles();
    let cycles_to_run = cycles + (1_790_000 / 60);

    while state.cpu.get_cycles() < cycles_to_run {
        state.cpu.step();

        let ppu_cycles = state.cpu.get_elapsed_cycles() * 3;
        for _ in 0..ppu_cycles {
            state.cpu.run_ppu_cycle();
        }
    }

    let fps = app.timer.fps();
    app.window().set_title(&format!("rnes - {} FPS", fps));
}

fn draw(_app: &mut App, gfx: &mut Graphics, plugins: &mut Plugins, state: &mut State) {
    if state.cpu.ppu_framebuffer_has_changed() {
        gfx.update_texture(&mut state.screen_tex)
            .with_data(&state.cpu.get_ppu_framebuffer())
            .update()
            .unwrap();
    }

    let mut output = plugins.egui(|ctx| {
        egui::Window::new("Screen")
            .title_bar(false)
            .collapsible(false)
            .resizable(false)
            .fixed_pos(egui::Pos2::new(5.0, 5.0))
            .show(ctx, |ui| {
                ui.image(state.screen_tex_id, state.screen_tex.size());
            });
    });

    output.clear_color(Color::BLACK);

    gfx.render(&output);

    // Draw image palette
    let mut draw = gfx.create_draw();

    let palettes = [
        state.cpu.ppu.get_image_palette(),
        state.cpu.ppu.get_sprite_palette(),
    ];

    for i in 0..2 {
        for j in 0..16 {
            let x = 11.0 + (j as f32 * 16.0);
            let y = 270.0 + (i as f32 * 18.0);

            let color_idx = palettes[i][j] as usize;
            let color_hex = ppu::PALETTE[color_idx] as u32;
            let color = Color::from_hex((color_hex << 8) | 0x000000FF);

            draw.rect((x, y), (16.0, 16.0)).color(color);
        }
    }

    gfx.render(&draw);
}
