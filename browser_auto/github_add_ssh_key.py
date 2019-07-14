#!/usr/bin/env python3.7
import datetime
import socket
from pathlib import Path

from selenium.webdriver.common.keys import Keys 

from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.common.by import By

from web_common import get_driver, get_pass


if __name__ == "__main__":
    driver = get_driver(headless=True)

    driver.get('https://github.com/login')
    driver.find_element_by_name('login').send_keys('znewman01')
    driver.find_element_by_name('password').send_keys(get_pass('github'))
    driver.find_element_by_name('commit').submit()

    otp_code = input("GitHub OTP? ").strip()
    otp_field = driver.find_element_by_name('otp')
    otp_field.send_keys(otp_code, Keys.ENTER)


    WebDriverWait(driver, 10).until(EC.url_to_be('https://github.com/'))

    driver.get('https://github.com/settings/ssh/new')

    hostname = socket.gethostname()
    timestamp = datetime.datetime.now().isoformat()
    driver.find_element_by_name('public_key[title]').send_keys(f'{hostname} {timestamp}')

    key_file = Path.home() / '.ssh' / 'id_rsa.pub'
    key_field = driver.find_element_by_name('public_key[key]')
    key_field.send_keys(key_file.read_text())

    key_field.submit()
    WebDriverWait(driver, 10).until(EC.url_to_be('https://github.com/settings/keys'))

    driver.quit()
