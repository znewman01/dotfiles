import re
import subprocess
import shutil
from pathlib import Path

from selenium.webdriver import Firefox, FirefoxProfile
from selenium.webdriver.firefox.options import Options

def get_profile_path():
    profiles_dir = Path.home() / '.mozilla' / 'firefox'
    return next(profiles_dir.rglob('*.default')).as_posix()

def keep_profile_changes():
    profile_path = get_profile_path() 
    selenium_profile_path = re.search(r'"(\S*rust_mozprofile\.[A-Za-z0-9]*)"',
                                      Path('geckodriver.log').read_text()).group(1)
    shutil.rmtree(profile_path)
    # shutil.copytree doesn't handle locks etc.
    subprocess.check_call(['cp', '-r', selenium_profile_path, profile_path])

def get_pass(passname):
    return subprocess.check_output(['pass', passname]).decode('utf-8').splitlines()[0].strip()

def get_driver(use_system_profile=False, headless=False):
    log_path = Path('geckodriver.log')
    if log_path.is_file():
        log_path.unlink()
    profile = FirefoxProfile(get_profile_path()) if use_system_profile else None
    options = Options()
    if headless:
         options.add_argument('-headless')
    driver = Firefox(executable_path='geckodriver',
                     firefox_profile=profile,
                     options=options)
    driver.implicitly_wait(10)
    return driver
