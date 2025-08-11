module utils

import os

// File utilities for LX compiler

// FileInfo represents information about a file
pub struct FileInfo {
pub:
	path          string
	name          string
	extension     string
	size          int
	modified_time i64
	exists        bool
}

// get_file_info gets information about a file
pub fn get_file_info(path string) FileInfo {
	if !os.exists(path) {
		return FileInfo{
			name:          os.file_name(path)
			extension:     os.file_ext(path)
			size:          0
			modified_time: 0
			exists:        false
		}
	}

	stat := os.stat(path) or {
		return FileInfo{
			name:          os.file_name(path)
			extension:     os.file_ext(path)
			size:          0
			modified_time: 0
			exists:        false
		}
	}

	return FileInfo{
		name:          os.file_name(path)
		extension:     os.file_ext(path)
		size:          int(stat.size)
		modified_time: stat.mtime
		exists:        true
	}
}

// read_file reads a file and returns its content
pub fn read_file(path string) string {
	if !os.exists(path) {
		return ''
	}

	content := os.read_file(path) or { return '' }
	return content
}

// read_file_lines reads a file and returns its lines
pub fn read_file_lines(path string) []string {
	content := read_file(path)
	if content.len == 0 {
		return []
	}
	return content.split('\n')
}

// write_file writes content to a file
pub fn write_file(path string, content string) bool {
	os.write_file(path, content) or { return false }
	return true
}

// write_file_lines writes lines to a file
pub fn write_file_lines(path string, lines []string) bool {
	content := lines.join('\n')
	return write_file(path, content)
}

// append_file appends content to a file
pub fn append_file(path string, content string) bool {
	os.write_file(path, content) or { return false }
	return true
}

// copy_file copies a file from source to destination
pub fn copy_file(source string, destination string) bool {
	if !os.exists(source) {
		return false
	}

	content := read_file(source)
	if content.len == 0 && os.file_size(source) > 0 {
		return false
	}

	return write_file(destination, content)
}

// move_file moves a file from source to destination
pub fn move_file(source string, destination string) bool {
	if !os.exists(source) {
		return false
	}

	os.rename(source, destination) or { return false }
	return true
}

// delete_file deletes a file
pub fn delete_file(path string) bool {
	if !os.exists(path) {
		return true
	}

	os.rm(path) or { return false }
	return true
}

// create_directory creates a directory
pub fn create_directory(path string) bool {
	if os.exists(path) {
		return true
	}

	os.mkdir_all(path) or { return false }
	return true
}

// list_files lists files in a directory
pub fn list_files(path string) []string {
	if !os.exists(path) {
		return []
	}

	files := os.ls(path) or { return [] }
	return files
}

// list_files_with_extension lists files with a specific extension
pub fn list_files_with_extension(path string, extension string) []string {
	files := list_files(path)
	mut filtered := []string{}

	for file in files {
		if os.file_ext(file) == extension {
			filtered << file
		}
	}

	return filtered
}

// find_lx_files finds all .lx files in a directory recursively
pub fn find_lx_files(path string) []string {
	mut files := []string{}

	if !os.exists(path) {
		return files
	}

	entries := os.ls(path) or { return files }

	for entry in entries {
		entry_path := os.join_path(path, entry)

		if os.is_dir(entry_path) {
			// Recursively search subdirectories
			subfiles := find_lx_files(entry_path)
			files << subfiles
		} else if os.file_ext(entry) == '.lx' {
			files << entry_path
		}
	}

	return files
}

// get_relative_path gets the relative path from base to target
pub fn get_relative_path(base string, target string) string {
	base_abs := os.abs_path(base)
	target_abs := os.abs_path(target)

	if !target_abs.starts_with(base_abs) {
		return target_abs
	}

	mut relative := target_abs[base_abs.len..]
	if relative.starts_with('/') {
		relative = relative[1..]
	}

	return relative
}

// ensure_directory ensures a directory exists, creating it if necessary
pub fn ensure_directory(path string) bool {
	if os.exists(path) {
		return os.is_dir(path)
	}

	return create_directory(path)
}

// get_temp_file creates a temporary file
pub fn get_temp_file(prefix string, suffix string) string {
	temp_dir := os.temp_dir()
	temp_file := os.join_path(temp_dir, '${prefix}_${os.getpid()}_${suffix}')
	return temp_file
}

// get_temp_directory creates a temporary directory
pub fn get_temp_directory(prefix string) string {
	temp_dir := os.temp_dir()
	temp_subdir := os.join_path(temp_dir, '${prefix}_${os.getpid()}')
	create_directory(temp_subdir)
	return temp_subdir
}

// clean_temp_files cleans up temporary files
pub fn clean_temp_files(prefix string) {
	temp_dir := os.temp_dir()
	files := list_files(temp_dir)

	for file in files {
		if file.starts_with(prefix) {
			file_path := os.join_path(temp_dir, file)
			delete_file(file_path)
		}
	}
}

// FileWatcher watches for file changes
pub struct FileWatcher {
pub:
	path     string
	callback ?fn (string)
pub mut:
	active bool
}

// new_file_watcher creates a new FileWatcher
pub fn new_file_watcher(path string, callback fn (string)) FileWatcher {
	return FileWatcher{
		path:     path
		callback: callback
		active:   false
	}
}

// start starts watching the file
pub fn (mut fw FileWatcher) start() {
	if fw.active {
		return
	}

	fw.active = true
	// TODO: Implement actual file watching
	// This would require platform-specific code or a library
}

// stop stops watching the file
pub fn (mut fw FileWatcher) stop() {
	fw.active = false
}

// is_active checks if the watcher is active
pub fn (fw FileWatcher) is_active() bool {
	return fw.active
}
