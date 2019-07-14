#!/usr/bin/env python3.7
import datetime
import socket
import time
from pathlib import Path

from selenium.webdriver.common.keys import Keys

from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.common.by import By

from web_common import get_driver, get_pass, do_mit_2fa

BASE_URLS = ['https://github.com', 'https://github.mit.edu']


def _is_on_page(driver, expected_text, css_selector='legend'):
    try:
        legend_text = driver.find_element_by_css_selector(css_selector).text
    except NoSuchElementException:
        return False
    return expected_text in legend_text

if __name__ == "__main__":
    driver = get_driver(headless=False, use_system_profile=True)
    wait = WebDriverWait(driver, 10)

    for base_url in BASE_URLS:

        driver.get(f'{base_url}/login')
        time.sleep(2)

        if 'mit' in base_url:
            if _is_on_page(driver, 'account provider'):
                driver.find_element_by_name('Select').submit()
                wait.until(EC.url_matches('https://idp.mit.edu/idp/Authn/MIT'))
            if _is_on_page(driver, 'MIT certificate?'):
                driver.find_element_by_name('login_certificate').submit()
                wait.until(EC.url_matches('https://idp.mit.edu:446/idp/Authn/Certificate'))
            if _is_on_page(driver, 'Duo Authentication', css_selector='h2'):
                do_mit_2fa(driver, f'{base_url}/')
        elif 'github' in base_url:
            if _is_on_page(driver, 'Sign in to GitHub', css_selector='.auth-form-header h1'):
                driver.find_element_by_name('login').send_keys('znewman01')
                driver.find_element_by_name('password').send_keys(get_pass('github'))
                driver.find_element_by_name('commit').submit()

                otp_code = input("GitHub OTP [empty to use SMS]? ").strip()
                if otp_code:
                    otp_field = driver.find_element_by_name('otp')
                    otp_field.send_keys(otp_code, Keys.ENTER)
                else:
                    driver.find_element_by_css_selector('.js-send-auth-code button').submit()
                    auth_code = input('SMS auth code? ').strip()
                    driver.find_element_by_name('otp').send_keys(auth_code, Keys.ENTER)


        wait.until(EC.url_to_be(f'{base_url}/'))

        driver.get(f'{base_url}/settings/ssh/new')

        hostname = socket.gethostname()
        timestamp = datetime.datetime.now().isoformat()
        driver.find_element_by_name('public_key[title]').send_keys(f'{hostname} {timestamp}')

        key_file = Path.home() / '.ssh' / 'id_rsa.pub'
        key_field = driver.find_element_by_name('public_key[key]')
        key_field.send_keys(key_file.read_text())

        key_field.submit()
        WebDriverWait(driver, 10).until(EC.url_to_be(f'{base_url}/settings/keys'))

    driver.quit()
