import arrays

struct ChoiceSettingType {
	choices []string
}

enum SimpleSettingType {
	string
	bool
	int
	text
}

type SettingType = ChoiceSettingType | SimpleSettingType
type SettingValue = bool | int | string

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
	default  ?SettingValue
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

	values map[string]SettingValue // A map of values
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

fn prv_cfg_get(config &Config, name string) SettingValue {
	return config.values[name] or { 'not found' }
}

[export: 'cfg_get']
fn cfg_get(config &Config, name &char) string {
	val := config.values[unsafe { name.vstring() }] or { 'no' }
	return match val {
		string {
			val
		}
		else {
			'no'
		}
	}
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
		setting_type: SimpleSettingType.string
	}
	schema.settings[s_name] = setting
	return setting
}

[export: 'cfg_add_integer_setting']
fn cfg_add_integer_setting(mut schema ConfigSchema, setting_name &char) &Setting {
	s_name := unsafe { setting_name.vstring() }
	setting := &Setting{
		name: s_name
		setting_type: SimpleSettingType.int
	}
	schema.settings[s_name] = setting
	return setting
}

[export: 'cfg_add_choice_setting']
fn cfg_add_choice_setting(mut schema ConfigSchema, setting_name &char, choices []&char) &Setting {
	s_name := unsafe { setting_name.vstring() }
	s_choices := arrays.map_indexed(choices, fn (idx int, choice &char) string {
		return unsafe { choice.vstring() }
	})
	setting := &Setting{
		name: s_name
		setting_type: ChoiceSettingType{s_choices}
	}
	schema.settings[s_name] = setting
	return setting
}

[export: 'cfg_setting_set_doc']
fn cfg_setting_set_doc(mut setting Setting, doc &char) {
	setting.doc = unsafe { doc.vstring() }
}

[export: 'cfg_setting_set_default']
fn cfg_setting_set_default(mut setting Setting, val SettingValue) {
	setting.default = val
}

[export: 'cfg_setting_set_integer_default']
fn cfg_setting_set_integer_default(mut setting Setting, val i64) {
	setting.default = int(val)
}

[export: 'cfg_setting_set_string_default']
fn cfg_setting_set_string_default(mut setting Setting, val &char) {
	setting.default = SettingValue(unsafe { val.vstring() })
}

fn print_setting_type(setting_type SettingType) {
	match setting_type {
		SimpleSettingType {
			match setting_type {
				.string { print('string') }
				.int { print('integer') }
				.bool { print('boolean') }
				.text { print('text') }
			}
		}
		ChoiceSettingType {
			print(setting_type.choices)
		}
	}
}

[export: 'cfg_cli_help']
fn cfg_cli_help(schema &ConfigSchema) {
	println(schema.name)
	println(schema.doc)

	println('Settings:')
	for name, setting in schema.settings {
		print('${name} - ${setting.setting_type}: ${setting.doc}')
		if setting.default != none {
			print('Default: ${setting.default.str()}')
			// print(setting)
		}
		println('')
	}
}

__global (
	cfg_validation_errors []string
)

[export: 'cfg_validate']
fn cfg_validate(config &Config) bool {
	if config.schema == none {
		return false
	}
	return true
}

[export: 'cfg_is_set']
fn cfg_is_set(config &Config, setting_name string) bool {
	return setting_name in config.values
}

[export: 'cfg_validate_with_schema']
fn cfg_validate_with_schema(config &Config, schema &ConfigSchema) bool {
	cfg_validation_errors = []
	for setting_name, setting in schema.settings {
		if setting.required && setting.default == none {
			if !cfg_is_set(config, setting_name) {
				// The setting is required but there's no value
				cfg_validation_errors << '${setting.name} is required'
			}
		}
		val := prv_cfg_get(config, setting_name)
		match val {
			string {
				match setting.setting_type {
					SimpleSettingType {
						match setting.setting_type {
							.string {}
							else {
								cfg_validation_errors << '${setting.name} if not a string'
							}
						}
					}
					else {
						cfg_validation_errors << '${setting.name} if not a string'
					}
				}
			}
			else {}
		}
	}

	return cfg_validation_errors.len != 0
}

[export: 'cfg_load_lib']
fn cfg_load_lib(argc int, argv []&char) {
}
