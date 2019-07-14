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

    driver.get('https://accounts.firefox.com/signin?service=sync&context=fx_desktop_v3&entrypoint=menupanel')
    driver.find_element_by_name('email').send_keys('firefox@z.znewman.net')
    driver.find_element_by_id('password').send_keys(get_pass('firefox'))
    driver.find_element_by_id('submit-btn').submit()

    print('You may have to go check your email and click a 2FA link...')
    WebDriverWait(driver, 60).until(EC.url_matches('https://accounts.firefox.com/connect_another_device'))
    print('2FA complete if applicable!')

    keep_profile_changes()
    driver.quit()
