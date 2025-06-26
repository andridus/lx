## [Unreleased]
### Fixed
- Fixed `utils.unescape_string` to use `result += s[i..i+1]` for correct string handling, ensuring all foundation tests pass and proper V idioms are followed.