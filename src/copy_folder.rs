use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::thread::{sleep, spawn};
use std::time::Duration;
use crate::{IS_COMPILED, main, PARSED_FILES};
use crate::add_types::utils::join;

pub struct CopyFolder {
    pub new_project_src: String,
    pub module_path: String,
}

impl Drop for CopyFolder {
    fn drop(&mut self) {
        /*1 delete_dir */ Command::new("rm")
            .arg("-rf")
            .arg(&self.new_project_src)
            .spawn().expect("rm command failed to start")
            .wait().expect("rm failed");
        if unsafe { IS_COMPILED } {
            let base_src = self.new_project_src.rsplit_once('/').unwrap().0;  // todo windows is \
            /*1 delete main */ Command::new("rm")
                .arg("main.rs")
                .current_dir(base_src)
                .spawn().expect("rm command failed to start")
                .wait().expect("rm failed");
            /*1 delete_dir */ Command::new("rm")
                .arg("-rf")
                .arg("target")
                .current_dir("/tmp/mold")
                .spawn().expect("rm command failed to start 1")
                .wait().expect("rm failed");
        }
    }
}

impl CopyFolder {
    pub fn start(&self) {
        let already_initialized = Path::new("/tmp/mold").exists();

        if !already_initialized {
            /*1 mkdir */ Command::new("mkdir")
                .arg("/tmp/mold")
                .spawn().expect("mkdir command failed to start")
                .wait().expect("mkdir failed");
        }
        /*1 mkdir */ Command::new("mkdir")
            .arg(&self.new_project_src)
            .spawn().expect("mkdir command failed to start")
            .wait().expect("mkdir failed");

        /*1 copy_dir */ Command::new("cp")
            .arg("-a")
            .arg(format!("{}/.", self.module_path))
            .arg(&self.new_project_src)
            .output().expect("cp command failed to start");
        if unsafe { IS_COMPILED } {
            if !already_initialized {
                /*1 make cargo.toml */
                Command::new("touch")
                    .arg("Cargo.toml")
                    .current_dir("/tmp/mold")
                    .spawn().expect("touch command failed to start")
                    .wait().expect("touch failed");
                fs::write("/tmp/mold/Cargo.toml", r#"[package]
                lto = "fat"
                codegen-units = 1
                name = "mold"
                version = "0.1.0"
                edition = "2021"

                [profile.release]
                overflow-checks = true

                [dependencies]
                list_comprehension_macro = "*""#).expect("Unable to write file");
            }
            /*1 make main */ Command::new("touch")
                .arg("main.rs")
                .current_dir(&self.new_project_src)
                .spawn().expect("touch command failed to start")
                .wait().expect("touch failed");
        } else {
            let path = PathBuf::from(&self.new_project_src);
            change_file_extensions(&path, "py");
        }
    }
}

pub fn delete_unused_files(path: &Path) {
    for child in fs::read_dir(path).unwrap() {
        let child = child.unwrap().path();
        if child.metadata().unwrap().is_file() {
            if matches!(child.extension(), Some(ex) if ex == "mo"){
                // 1 annoying lint
                if unsafe {
                    !PARSED_FILES.contains_key(child.as_path().to_str().unwrap())
                } {
                    fs::remove_file(child).unwrap();
                }
            }
        } else {
            delete_unused_files(&child);
        }
    }
}
pub fn change_file_extensions(path: &Path, new_ex: &str) -> bool {
    let mut file_names = vec![];
    for child in fs::read_dir(path).unwrap() {
        let mut child = child.unwrap().path();
        if child.metadata().unwrap().is_file() {
            if matches!(child.extension(), Some(ex) if ex == "mo") {
                if unsafe { IS_COMPILED } {
                    file_names.push(
                        child.file_name().unwrap().to_str().unwrap()
                            .strip_suffix(".mo").unwrap().to_string()
                    );
                }
                fs::rename(child.clone().to_str().unwrap(), {
                    child.set_extension(new_ex);
                    child
                }).unwrap();
            }
        } else {
            let is_mod = change_file_extensions(&child, new_ex);
            if is_mod {
                file_names.push(child.file_name().unwrap().to_str().unwrap().to_string())
            }
        }
    }
    if !file_names.is_empty() {
        let mut mod_file = File::create(format!("{}/mod.rs", path.to_str().unwrap())).unwrap();
        mod_file.write_all(join(
            file_names.iter().map(|name| format!("pub mod {name};")),
            "\n"
        ).as_ref()).unwrap();
        true
    } else { false }
}