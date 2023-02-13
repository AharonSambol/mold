use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use crate::construct_ast::ast_structure::join;
use crate::IS_COMPILED;

pub struct CopyFolder {
    pub temp_path: String,
    pub module_path: String,
}
impl Drop for CopyFolder {
    fn drop(&mut self) {
        if unsafe { IS_COMPILED } {
            /*1 delete_dir */ Command::new("rm")
                .arg("-rf")
                .arg(&self.temp_path)
                .spawn().expect("rm command failed to start")
                .wait().expect("rm failed");
            /*1 delete main */ Command::new("rm")
                .arg("main.rs")
                .current_dir(self.temp_path.rsplit_once('/').unwrap().0) // todo windows is \
                .spawn().expect("touch command failed to start")
                .wait().expect("touch failed");
        } else {
            /*1 delete_dir */ Command::new("rm")
                .arg("-rf")
                .arg(&self.module_path)
                .spawn().expect("rm command failed to start")
                .wait().expect("rm failed");
            /*1 un_renamed_dir */ Command::new("mv")
                .arg(&self.temp_path)
                .arg(&self.module_path)
                .spawn().expect("mv command failed to start")
                .wait().expect("mv failed");
        }
    }
}
impl CopyFolder {
    pub fn start(&self, file_path: &str) {
        if unsafe { IS_COMPILED } {
            /*1 mkdir */ Command::new("mkdir")
                .arg(&self.temp_path)
                .spawn().expect("mkdir command failed to start")
                .wait().expect("mkdir failed");
            /*1 copy_dir */ Command::new("cp")
                .arg("-a")
                .arg(format!("{}/.", self.module_path))
                .arg(format!("{}/", self.temp_path))
                .output().expect("cp command failed to start");
            let path = PathBuf::from(&self.temp_path);
            change_file_extensions(&path, "rs");
            let src = self.temp_path.rsplit_once('/').unwrap().0;
            /*1 make main */ Command::new("touch")
                .arg("main.rs")
                .current_dir(src)
                .spawn().expect("touch command failed to start")
                .wait().expect("touch failed");
            let mut main = File::create(format!("{src}/main.rs")).unwrap(); // todo windows is \
            let module_name = self.temp_path.as_str().rsplit_once('/').unwrap().1; // todo windows is \
            let file_name = file_path
                .strip_prefix(&self.temp_path).unwrap()
                .strip_prefix('/').unwrap()
                .replace('/', "::"); // todo windows is \
            let file_name = file_name.strip_suffix(".rs").unwrap();
            main.write_all(format!(
                "mod {module_name}; fn main() {{ {module_name}::{file_name}::main(); }}"
            ).as_ref()).unwrap();
        } else {
            println!("module_path: {:?}", self.module_path);
            println!("temp_path: {:?}", self.temp_path);
            /*1 mkdir */ Command::new("mkdir")
                .arg(&self.temp_path)
                .spawn().expect("ls command failed to start")
                .wait().expect("mkdir failed");
            /*1 copy_dir */ Command::new("cp")
                .arg("-a")
                .arg(format!("{}/.", self.module_path))
                .arg(format!("{}/", self.temp_path))
                .output().expect("ls command failed to start");
            let path = PathBuf::from(&self.module_path);
            change_file_extensions(&path, "py");
        }
    }
}

fn change_file_extensions(path: &Path, new_ex: &str) -> bool {
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