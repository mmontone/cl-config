struct StringSettingType {}

struct IntegerSettingType {}

struct BooleanSettingType {}

struct TextSettingType {}

struct ChoiceSettingType {
	choices []string
}

type SettingType = BooleanSettingType
	| ChoiceSettingType
	| IntegerSettingType
	| StringSettingType
	| TextSettingType

struct SettingGroup {
	name string
mut:
	doc string

	settings map[string]&Setting
}

struct Setting {
	name         string
	setting_type SettingType
mut:
	required bool
	doc      string
}

struct ConfigSchema {
	name string
mut:
	parent ?&Config
	doc    string

	settings   map[string]&Setting
	subschemas map[string]&ConfigSchema
}

struct Config {
	name string
mut:
	parent ?&Config
	schema ?&ConfigSchema
	doc    string // Configuration documentation

	values map[string]string // A map of values
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
	return config.values[unsafe { name.vstring() }]
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

[export: 'cfg_create_schema']
fn cfg_create_schema(name &char) &ConfigSchema {
	return &ConfigSchema{
		name: unsafe { name.vstring() }
	}
}

[export: 'cfg_set_schema_doc']
fn cfg_set_schema_doc(mut schema ConfigSchema, doc &char) {
	schema.doc = unsafe { doc.vstring() }
}

[export: 'cfg_add_string_setting']
fn cfg_add_string_setting(mut schema ConfigSchema, setting_name &char) &Setting {
	s_name := unsafe { setting_name.vstring() }
	setting := &Setting{
		name: s_name
		setting_type: StringSettingType{}
	}
	schema.settings[s_name] = setting
	return setting
}

[export: 'cfg_setting_set_doc']
fn cfg_setting_set_doc(mut setting Setting, doc &char) {
	setting.doc = unsafe { doc.vstring() }
}

[export: 'cfg_cli_help']
fn cfg_cli_help(schema &ConfigSchema) {
	println(schema.name)
	println(schema.doc)
}

[export: 'cfg_validate']
fn cfg_validate(config &Config) bool {
	return true
}

[export: 'cfg_validate_with_schema']
fn cfg_validate_with_schema(config &Config, schema &ConfigSchema) bool {
	return true
}

[export: 'cfg_load_lib']
fn cfg_load_lib(argc int, argv []&char) {
}
