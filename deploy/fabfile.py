from fabric.api import local, put, env, run, get, sudo

# fab host_build build
# fab host_prod deploy

env.use_ssh_config = True

def host_build():
  env.user = 'vagrant'
  env.hosts = ['hackage.build']

def host_prod():
  env.user = 'ubuntu'
  env.hosts = ['hackage.prod']



def setup_dev():
  print("Hello World!")

def setup_build():
  # install ghc
  # install cabal-install
  # cabal install hsenv
  # install lib-pgdev
  # cabal install --only-dependencies
  # cabal configure
  # cabal build
  print("Hello World!")

def setup_prod():
  put('angel', mode=755)
  put('angel.conf')
  # run('./angel angel.conf&')



def build():
  # upload build dependencies
  put('../src', '~/', mirror_local_mode=True)
  put('../hackage-explorer.cabal', '~/', mirror_local_mode=True)

  # build
  run('cabal install --only-dependencies')
  run('cabal configure')
  run('cabal build')
  run('strip dist/build/hackage-explorer/hackage-explorer')

  # get binary
  get('dist/build/hackage-explorer/hackage-explorer', '.')

def deploy():
  sudo('kill `cat hackage-explorer.pid`')
  local('chmod 755 hackage-explorer')
  put('hackage-explorer', '~/', mirror_local_mode=True)
  # put('static', '~/')

# def restart():
  # run('./hackage-explorer&')
  # auto restart by Angel

