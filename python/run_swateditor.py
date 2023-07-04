'''----- run swatpluseditor.exe to create SWAT text I/O files ------'''

### set control flag allowing this step to be skipped
##skip_editor = False
##if any('skip_editor' == cfg['name']):
##    skip_editor = cfg.loc[cfg['name'] == 'skip_editor', 'file'].values[0]
##    skip_editor = skip_editor == 'TRUE'
##if skip_editor:
##    sys.exit(1)
##
### identify weather inputs directory and construct strings for command line
##weather_path = Path(cfg.loc[cfg['name'] == 'wdat', 'file'].values[0])
##weather_str = '"' + str(weather_path) + '"'
##editor_exe_str = '"' + str(editor_exe_path) + '"'
##project_sqlite_str = '"' + str(project_sqlite_path) + '"'
##
### build list of command-line arguments
##editor_cli_args = [
##    '--cmd-only',
##    '--weather-dir', weather_str,
##    '--weather-import-format', 'old',
##    '--year-start', cfg.loc[cfg['name'] == 'start_yr', 'file'].values[0],
##    '--day-start', cfg.loc[cfg['name'] == 'start_day', 'file'].values[0],
##    '--year-end', cfg.loc[cfg['name'] == 'end_yr', 'file'].values[0],
##    '--day-end', cfg.loc[cfg['name'] == 'end_day', 'file'].values[0]
##]
##
### build the full command line call string and execute
##print('\n>> setting up project in SWATEditor')
##cli_string = ' '.join([editor_exe_str, project_sqlite_str] + editor_cli_args)
##subprocess.call(cli_string)

# finish
sys.exit(1)
