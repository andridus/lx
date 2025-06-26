module utils

import time

// Debug utilities for LX compiler

// DebugLevel represents the level of debug information
pub enum DebugLevel {
	none
	error
	warning
	info
	debug
	trace
}

// str returns a string representation of DebugLevel
pub fn (dl DebugLevel) str() string {
	return match dl {
		.none { 'NONE' }
		.error { 'ERROR' }
		.warning { 'WARNING' }
		.info { 'INFO' }
		.debug { 'DEBUG' }
		.trace { 'TRACE' }
	}
}

// DebugInfo represents debug information
pub struct DebugInfo {
pub:
	level DebugLevel
	message string
	context string
	timestamp time.Time
	component string
	file string
	line int
}

// new_debug_info creates a new DebugInfo
pub fn new_debug_info(level DebugLevel, message string, context string) DebugInfo {
	return DebugInfo{
		level: level
		message: message
		context: context
		timestamp: time.now()
		component: ''
		file: ''
		line: 0
	}
}

// new_debug_info_with_location creates a new DebugInfo with location
pub fn new_debug_info_with_location(level DebugLevel, message string, context string, component string, file string, line int) DebugInfo {
	return DebugInfo{
		level: level
		message: message
		context: context
		timestamp: time.now()
		component: component
		file: file
		line: line
	}
}

// str returns a string representation of DebugInfo
pub fn (di DebugInfo) str() string {
	mut result := '[${di.timestamp.format_ss_micro()} ${di.level.str()}]'

	if di.component.len > 0 {
		result += ' [${di.component}]'
	}

	if di.file.len > 0 {
		result += ' ${di.file}:${di.line}'
	}

	result += ': ${di.message}'

	if di.context.len > 0 {
		result += ' (${di.context})'
	}

	return result
}

// DebugLogger handles debug logging
pub struct DebugLogger {
mut:
	level DebugLevel
	enabled bool
	logs []DebugInfo
	max_logs int
}

// new_debug_logger creates a new DebugLogger
pub fn new_debug_logger(level DebugLevel) DebugLogger {
	return DebugLogger{
		level: level
		enabled: true
		logs: []
		max_logs: 1000
	}
}

// set_level sets the debug level
pub fn (mut dl DebugLogger) set_level(level DebugLevel) {
	dl.level = level
}

// enable enables debug logging
pub fn (mut dl DebugLogger) enable() {
	dl.enabled = true
}

// disable disables debug logging
pub fn (mut dl DebugLogger) disable() {
	dl.enabled = false
}

// log logs a debug message
pub fn (mut dl DebugLogger) log(level DebugLevel, message string, context string) {
	if !dl.enabled || int(level) < int(dl.level) {
		return
	}

	debug_info := new_debug_info(level, message, context)
	dl.logs << debug_info
}

// log_with_location logs a debug message with location
pub fn (mut dl DebugLogger) log_with_location(level DebugLevel, message string, context string, component string, file string, line int) {
	if !dl.enabled || int(level) < int(dl.level) {
		return
	}

	debug_info := new_debug_info_with_location(level, message, context, component, file, line)
	dl.logs << debug_info
}

// error logs an error message
pub fn (mut dl DebugLogger) error(message string, context string) {
	dl.log(.error, message, context)
}

// warning logs a warning message
pub fn (mut dl DebugLogger) warning(message string, context string) {
	dl.log(.warning, message, context)
}

// info logs an info message
pub fn (mut dl DebugLogger) info(message string, context string) {
	dl.log(.info, message, context)
}

// debug logs a debug message
pub fn (mut dl DebugLogger) debug(message string, context string) {
	dl.log(.debug, message, context)
}

// trace logs a trace message
pub fn (mut dl DebugLogger) trace(message string, context string) {
	dl.log(.trace, message, context)
}

// get_logs returns all logs
pub fn (dl DebugLogger) get_logs() []DebugInfo {
	return dl.logs.clone()
}

// get_logs_by_level returns logs filtered by level
pub fn (dl DebugLogger) get_logs_by_level(level DebugLevel) []DebugInfo {
	mut filtered := []DebugInfo{}
	for log in dl.logs {
		if log.level == level {
			filtered << log
		}
	}
	return filtered
}

// clear_logs clears all logs
pub fn (mut dl DebugLogger) clear_logs() {
	dl.logs.clear()
}

// format_debug formats debug information for display
pub fn format_debug(debug_info DebugInfo) string {
	return debug_info.str()
}

// PerformanceTimer tracks performance metrics
pub struct PerformanceTimer {
mut:
	name string
	start_time time.Time
	end_time time.Time
	running bool
}

// new_performance_timer creates a new PerformanceTimer
pub fn new_performance_timer(name string) PerformanceTimer {
	return PerformanceTimer{
		name: name
		start_time: time.Time{}
		end_time: time.Time{}
		running: false
	}
}

// start starts the timer
pub fn (mut pt PerformanceTimer) start() {
	pt.start_time = time.now()
	pt.running = true
}

// stop stops the timer
pub fn (mut pt PerformanceTimer) stop() {
	if pt.running {
		pt.end_time = time.now()
		pt.running = false
	}
}

// get_duration_milliseconds returns the duration in milliseconds
pub fn (pt PerformanceTimer) get_duration_milliseconds() i64 {
	if pt.running {
		return time.since(pt.start_time).milliseconds()
	}
	return pt.end_time.unix_milli() - pt.start_time.unix_milli()
}

// get_duration_microseconds returns the duration in microseconds
pub fn (pt PerformanceTimer) get_duration_microseconds() i64 {
	if pt.running {
		return time.since(pt.start_time).microseconds()
	}
	return pt.end_time.unix_micro() - pt.start_time.unix_micro()
}

// get_duration returns the duration in seconds as f64
pub fn (pt PerformanceTimer) get_duration() f64 {
	return f64(pt.get_duration_milliseconds()) / 1000.0
}

// str returns a string representation of the timer
pub fn (pt PerformanceTimer) str() string {
	duration := pt.get_duration()
	status := if pt.running { 'running' } else { 'stopped' }
	return '${pt.name}: ${duration:.2f}ms (${status})'
}

// PerformanceTracker tracks multiple performance timers
pub struct PerformanceTracker {
mut:
	timers map[string]PerformanceTimer
}

// new_performance_tracker creates a new PerformanceTracker
pub fn new_performance_tracker() PerformanceTracker {
	return PerformanceTracker{
		timers: map[string]PerformanceTimer{}
	}
}

// start_timer starts a new timer
pub fn (mut pt PerformanceTracker) start_timer(name string) {
	mut timer := new_performance_timer(name)
	timer.start()
	pt.timers[name] = timer
}

// stop_timer stops a timer with the given name
pub fn (mut pt PerformanceTracker) stop_timer(name string) {
	if name in pt.timers {
		mut timer := pt.timers[name]
		timer.stop()
		pt.timers[name] = timer
	}
}

// get_timer returns a timer by name
pub fn (pt PerformanceTracker) get_timer(name string) ?PerformanceTimer {
	if name in pt.timers {
		return pt.timers[name]
	}
	return none
}

// get_all_timers returns all timers
pub fn (pt PerformanceTracker) get_all_timers() []PerformanceTimer {
	return pt.timers.values()
}

// get_summary returns a summary of all timers
pub fn (pt PerformanceTracker) get_summary() string {
	mut summary := 'Performance Summary:\n'

	for timer in pt.timers.values() {
		summary += '  ${timer.str()}\n'
	}

	return summary
}

// clear_timers clears all timers
pub fn (mut pt PerformanceTracker) clear_timers() {
	pt.timers.clear()
}

// DebugContext provides context for debugging
pub struct DebugContext {
pub:
	component string
	file string
	line int
	function string
}

// new_debug_context creates a new DebugContext
pub fn new_debug_context(component string, file string, line int, function string) DebugContext {
	return DebugContext{
		component: component
		file: file
		line: line
		function: function
	}
}

// str returns a string representation of DebugContext
pub fn (dc DebugContext) str() string {
	return '${dc.component}:${dc.file}:${dc.line}:${dc.function}'
}
