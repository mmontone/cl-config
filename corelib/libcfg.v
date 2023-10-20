struct Config {
	name string
mut:
	values map[string]string
}

[export: 'cfg_create']
fn cfg_create(name &char) &Config {
	return &Config{
		name: unsafe { name.vstring() }
	}
}

[export: 'cfg_name']
fn cfg_name(config &Config) string {
	return config.name
}

[export: 'cfg_get']
fn cfg_get(config &Config, name &char) string {
	return config.values[unsafe {name.vstring()}]
}

[export: 'cfg_set']
fn cfg_set(mut config Config, name &char, value &char) {
	config.values[unsafe { name.vstring() }] = unsafe { value.vstring() }
}

[export: 'cfg_print']
fn cfg_print(config &Config) {
	println(config.name)
	for key, value in config.values {
		print(key)
		print(': ')
		println(value)
	}
}

[export: 'cfg_load_lib']
fn cfg_load_lib(argc int, argv []&char) {
}
