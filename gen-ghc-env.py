#!/usr/bin/env nix-shell
#!nix-shell -i python -p python3

import subprocess, ast, re, argparse, sys

def run_debug(*args):
    print('$', ' '.join(repr(a) if len(a.split()) > 1 else a for a in args))
    return subprocess.check_output(args).strip().decode('utf8')

def ghc_pkg_dump():
    for pkg in run_debug('ghc-pkg', 'dump').split('---'):
        parts = list(filter(None, map(str.strip, re.split(r'^([\w-]+):', pkg, flags=re.MULTILINE))))
        keys = parts[0::2]
        vals = parts[1::2]
        assert len(keys) == len(vals)
        yield dict(zip(keys, vals))

def main(opts):
    print(opts)
    ignores = frozenset(opts.ignored_packages)
    ghc_info = dict(ast.literal_eval(run_debug('ghc', '--info')))
    ghc_ver = ghc_info['Project version']
    arch, _, os = ghc_info['target platform string'].split('-')
    with open('.ghc.environment.{arch}-{os}-{ghc_ver}'.format(**locals()), encoding='utf8', mode='w') as fd:
        print('truncated', fd.name)
        fd.write('clear-package-db\n')
        fd.write('global-package-db\n')
        [fd.write('package-db {}\n'.format(db)) for db in sorted(run_debug('ghc-pkg', 'list').split('\n')) if db.strip() and not db.startswith(' ')]
        [fd.write('package-id {}\n'.format(pkg['id'])) for pkg in sorted(ghc_pkg_dump(), key=lambda p: p['name']) if pkg['name'] not in ignores]

def parse(argv):
    ap = argparse.ArgumentParser()
    ap.add_argument('ignored_packages', metavar='ignore-pkg', nargs='*', help='leave any package matching these names out of the final list')
    return ap.parse_args(argv)

if __name__ == '__main__':
    main(parse(sys.argv[1:]))
