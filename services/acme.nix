{ config, ... }:

{
  security.acme = {
    acceptTerms = true;
    email = "letsencrypt@z.znewman.net";
  };
}
