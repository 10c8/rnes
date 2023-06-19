use std::{
    env, fs,
    path::{Path, PathBuf},
};

fn get_target_dir() -> PathBuf {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    let build_type = env::var("PROFILE").unwrap();
    let path = Path::new(&manifest_dir).join("target").join(build_type);
    return path.into();
}

fn copy_dir<P, Q>(from: P, to: Q)
where
    P: AsRef<Path>,
    Q: AsRef<Path>,
{
    let to = to.as_ref().to_path_buf();

    for path in fs::read_dir(from).unwrap() {
        let path = path.unwrap().path();
        let to = to.clone().join(path.file_name().unwrap());

        if path.is_file() {
            fs::copy(&path, to).unwrap();
        } else if path.is_dir() {
            if !to.exists() {
                fs::create_dir(&to).unwrap();
            }

            copy_dir(&path, to);
        }
    }
}

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=resources");

    let target_dir = get_target_dir();

    if env::var("PROFILE").unwrap() == "release" {
        let resources_dir = Path::new("resources");
        let target_resources_dir = target_dir.join("resources");

        if target_resources_dir.exists() {
            fs::remove_dir_all(&target_resources_dir).unwrap();
        }

        fs::create_dir(&target_resources_dir).unwrap();
        copy_dir(&resources_dir, &target_resources_dir);
    }
}
