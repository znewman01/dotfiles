import configparser
import re
import subprocess
import shutil
import time
from pathlib import Path

from selenium.webdriver import Firefox, FirefoxProfile
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.firefox.options import Options
from selenium.webdriver.support.ui import WebDriverWait

def get_profile_path():
    firefox_dir = Path.home() /'.mozilla' / 'firefox'
    config = configparser.ConfigParser()
    assert config.read((firefox_dir / 'profiles.ini'))
    for section in dict(config).values():
        if section.get('default') == '1':
            return (firefox_dir / section.get('path')).as_posix()

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

def do_mit_2fa(driver, expected_url):
    driver.switch_to.frame('duo_iframe')
    time.sleep(2)  # the phone button is clickable before it actually works
    driver.find_element_by_css_selector('.phone-label button').click()

    driver.switch_to.default_content()

    print("Pick up the phone and hit a key!")
    # Wait long since this could take a while...
    WebDriverWait(driver, 60).until(EC.url_to_be(expected_url))
