#!/usr/bin/env python3.7
import time

from selenium.webdriver.common.keys import Keys 
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.common.by import By
from selenium.webdriver.common.alert import Alert

from web_common import get_driver, get_pass, keep_profile_changes


if __name__ == "__main__":
    driver = get_driver(use_system_profile=True, headless=True)
    wait = WebDriverWait(driver, 10)

    driver.get('https://ca.mit.edu')

    driver.find_element_by_name('login').send_keys('zjn')
    driver.find_element_by_name('password').send_keys(get_pass('mit'))
    driver.find_element_by_name('Submit').submit()

    driver.switch_to.frame('duo_iframe')
    time.sleep(2)  # the phone button is clickable before it actually works
    driver.find_element_by_css_selector('.phone-label button').click()

    driver.switch_to.default_content()

    print("Pick up the phone and hit a key!")
    # Wait long since this could take a while...
    WebDriverWait(driver, 60).until(EC.url_to_be('https://ca.mit.edu/ca/certgen'))

    wait.until(EC.element_to_be_clickable((By.NAME, 'Submit'))).submit()
    wait.until(EC.alert_is_present())
    Alert(driver).accept()
    wait.until(EC.url_to_be('https://ca.mit.edu/ca/mozcert/0'))

    keep_profile_changes()
    driver.quit()
