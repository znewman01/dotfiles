#! /usr/bin/env nix-shell
#! nix-shell --pure -i python -p "python.withPackages(ps: [ps.requests])" pass
import requests
import sys
import subprocess

IP_USERNAME = 'znewman01@gmail.com'

class Instapaper(object):
  def __init__(self, username, password):
    self._username = username
    self._password = password

  def _get_auth(self):
    return requests.auth.HTTPBasicAuth(self._username, self._password)

  def add(self, url):
    response = requests.get('https://www.instapaper.com/api/add',
                            auth=self._get_auth(), params={'url': url})
    response.raise_for_status()


def main():
  url = sys.argv[1]
  password = subprocess.check_output(['pass', 'show', 'instapaper']).strip()
  ip_api = Instapaper(IP_USERNAME, password)
  ip_api.add(url)


if __name__ == '__main__':
  main()
