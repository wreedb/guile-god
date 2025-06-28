project(
    'guile-god',
    version: '0.1.0',
    license: 'GPL-3.0-or-later',
    meson_version: '>= 1.1'
)

guile = find_program('guile', required: true)
guild = find_program('guild', required: true)
guile_major_version = run_command(guile, '-c', '(display (major-version))', check: true).stdout().strip()
guile_version = run_command(guile, '-c', '(display (version))', check: true).stdout().strip()

if guile_major_version != '3'
    error('guile version 3 (or later) is required, found version' + guile_version)
endif

message('found guile version: ' + guile_version)
  
prefix = get_option('prefix')
libdir = get_option('libdir')
datadir = get_option('datadir')

guile_site = get_option('guile-site')
guile_ccache = get_option('guile-ccache')

if guile_site != ''
    guile_site_dir = guile_site
else
    guile_site_dir = prefix / datadir / 'guile' / 'site' / '3.0'
endif

if guile_ccache != ''
    guile_ccache_dir = guile_ccache
else
    guile_ccache_dir = prefix / libdir / 'guile' / '3.0' / 'site-ccache'
endif

guile_sources = [
    'src/god/token.scm',
    'src/god/parse.scm',
    'src/god/query.scm',
    'src/god/convert.scm'
]

compiled_files = []

foreach scmfile : guile_sources
    rel_path = scmfile.split('src/')[1]
    if rel_path.contains('/')
        subdir_parts = rel_path.split('/')
        fname = subdir_parts.get(-1)

        subdir_list = []
        foreach i : range(subdir_parts.length() - 1)
            subdir_list += subdir_parts.get(i)
        endforeach
        subdir = '/'.join(subdir_list)

        install_data(scmfile, install_dir: guile_site_dir / subdir)
        output_name = fname.replace('.scm', '.go')
        compiled = custom_target('compile_' + rel_path.underscorify(),
            input: scmfile,
            output: output_name,
            command: [guild, 'compile', '-O2', '-o', '@OUTPUT@', '@INPUT@'],
            install: true,
            install_dir: guile_ccache_dir / subdir)
    endif
endforeach
