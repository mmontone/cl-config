* Deliver core as a shared library.

  - Deliver core as a shared library so that this can be used from any programming language.
  - Possible name: libconfig.
  - May have to implement using another language, as CL binaries are too big. Consider ECL, or some Scheme. 
  Candidate languages for core: Plain C, V lang, Janet. 
  Candidate languages for command line utitilies and GUI: Scheme implementation.
  
* A command line interface frontend.

  - Both interactive, and with command line processing. Completion.
  - Run a "config" binary in some directory, that opens an interactive configuration manager:
      Possible commands:
        - load(config)
        - save(config)
        - set(option, value)
        - get(option)
        - help(config)
        - setconfig(config) - Set the current config
        - ...

* Create command line options parsing from configuration schemas.

  - Print command line help from schema.
  - Create a configuration from a schema, from command line.
  - Parse configuration options as command line arguments, like: --web.host=...
  - Command line completion based on schema.

* Maybe JSON and YAML backends.


