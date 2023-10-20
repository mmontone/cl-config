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

	settings []&Setting
}

struct Setting {
	name         string
	setting_type string
}

type ConfigSchemaSetting = Setting | SettingGroup

struct ConfigSchema {
	name string
mut:
	parent ?&Config
	doc    string

	settings []&ConfigSchemaSetting
}

struct Config {
	name string
mut:
	parent ?&Config
	schema ?&ConfigSchema
	doc    string

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
