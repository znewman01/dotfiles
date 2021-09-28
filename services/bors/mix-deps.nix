{ lib, beamPackages, overrides ? (x: y: {}) }:

let
	buildRebar3 = lib.makeOverridable beamPackages.buildRebar3;
	buildMix = lib.makeOverridable beamPackages.buildMix;
	buildErlangMk = lib.makeOverridable beamPackages.buildErlangMk;

	self = packages // (overrides self packages);

	packages = with beamPackages; with self; {
		decimal = buildMix rec {
			name = "decimal";
			version = "2.0.0";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "0xzm8hfhn8q02rmg8cpgs68n5jz61wvqg7bxww9i1a6yanf6wril";
			};

			beamDeps = [];
		};

		ssl_verify_fun = buildRebar3 rec {
			name = "ssl_verify_fun";
			version = "1.1.6";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "1026l1z1jh25z8bfrhaw0ryk5gprhrpnirq877zqhg253x3x5c5x";
			};

			beamDeps = [];
		};

		hackney = buildRebar3 rec {
			name = "hackney";
			version = "1.17.4";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "05kbk3rpw2j3cb9pybikydxmi2nm5pidpx0jsm48av2mjr4zy5ny";
			};

			beamDeps = [ certifi idna metrics mimerl parse_trans ssl_verify_fun unicode_util_compat ];
		};

		nimble_parsec = buildMix rec {
			name = "nimble_parsec";
			version = "1.1.0";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "0jzmr0x3s6z16c74gvrk9aqc10brn6a1dwa8ywzr2vkhdgb35sq8";
			};

			beamDeps = [];
		};

		phoenix_html = buildMix rec {
			name = "phoenix_html";
			version = "3.0.3";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "1pidqrz17zjl33n0624v189jjzmvqkcbivb1jxjha1y6x3ljl5g8";
			};

			beamDeps = [ plug ];
		};

		poison = buildMix rec {
			name = "poison";
			version = "5.0.0";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "1z6kv2s6w5nrq20446510nys30ir0hfr8ksrlxi0rf01qlbn3p0i";
			};

			beamDeps = [ decimal ];
		};

		jason = buildMix rec {
			name = "jason";
			version = "1.2.2";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "0y91s7q8zlfqd037c1mhqdhrvrf60l4ax7lzya1y33h5y3sji8hq";
			};

			beamDeps = [ decimal ];
		};

		makeup = buildMix rec {
			name = "makeup";
			version = "1.0.5";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "1a9cp9zp85yfybhdxapi9haa1yykzq91bw8abmk0qp1z5p05i8fg";
			};

			beamDeps = [ nimble_parsec ];
		};

		bunt = buildMix rec {
			name = "bunt";
			version = "0.2.0";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "0lw3v9kwbbcy1v6ygziiky887gffwwmxvyg4r1v0zm71kzhcgxbs";
			};

			beamDeps = [];
		};

		mimerl = buildRebar3 rec {
			name = "mimerl";
			version = "1.2.0";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "08wkw73dy449n68ssrkz57gikfzqk3vfnf264s31jn5aa1b5hy7j";
			};

			beamDeps = [];
		};

		erlex = buildMix rec {
			name = "erlex";
			version = "0.2.6";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "0x8c1j62y748ldvlh46sxzv5514rpzm809vxn594vd7y25by5lif";
			};

			beamDeps = [];
		};

		cowlib = buildErlangMk rec {
			name = "cowlib";
			version = "1.0.2";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "1lmbwr22gcmcl643gqq5579r10fchq8y6lxrd8vfcfd07ah2sqnv";
			};

			beamDeps = [];
		};

		myxql = buildMix rec {
			name = "myxql";
			version = "0.5.1";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "1ms8pjsamx0z3fdlpra8wwg63v0h2igihpvmg9qgk7hims2bdikk";
			};

			beamDeps = [ db_connection decimal jason ];
		};

		plug_cowboy = buildMix rec {
			name = "plug_cowboy";
			version = "1.0.0";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "1a79p4gcaqaa829zw1vm69ij5s7m8n5sjr94ps1l87wag9103lh1";
			};

			beamDeps = [ cowboy plug ];
		};

		unicode_util_compat = buildRebar3 rec {
			name = "unicode_util_compat";
			version = "0.7.0";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "08952lw8cjdw8w171lv8wqbrxc4rcmb3jhkrdb7n06gngpbfdvi5";
			};

			beamDeps = [];
		};

		cowboy = buildErlangMk rec {
			name = "cowboy";
			version = "1.1.2";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "0z7inhdsljbkqmbr2hx6mzz7rhbiip64zg14dznwwgi312z3nxpl";
			};

			beamDeps = [ cowlib ranch ];
		};

		metrics = buildRebar3 rec {
			name = "metrics";
			version = "1.0.1";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "05lz15piphyhvvm3d1ldjyw0zsrvz50d2m5f2q3s8x2gvkfrmc39";
			};

			beamDeps = [];
		};

		ecto = buildMix rec {
			name = "ecto";
			version = "3.7.1";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "0f463fw0mydnk7vsy7rinsly85lpbn3f3nylzx66b7j7zhwmnvnk";
			};

			beamDeps = [ decimal jason telemetry ];
		};

		credo = buildMix rec {
			name = "credo";
			version = "1.5.6";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "05zbqwzpai3257slw5ibblz6wazallc8ar1awq6y6r5xb3js6ljb";
			};

			beamDeps = [ bunt file_system jason ];
		};

		ranch = buildRebar3 rec {
			name = "ranch";
			version = "1.3.2";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "0zrm3h3sflhrj7xydqznxn70swnn8v4jac6a7b6gqcr4hqx4jmkf";
			};

			beamDeps = [];
		};

		ex_link_header = buildMix rec {
			name = "ex_link_header";
			version = "0.0.5";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "1zhkb86cdlrswhmiarm984rdahpq22grlvl7x5fdp9zybhq6pkfk";
			};

			beamDeps = [];
		};

		tesla = buildMix rec {
			name = "tesla";
			version = "1.4.3";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "11h3fnsmkwjbhvs70rjqljfzinvsr4hg6c99yx56ckdzcjv5nxg0";
			};

			beamDeps = [ hackney jason mime poison telemetry ];
		};

		certifi = buildRebar3 rec {
			name = "certifi";
			version = "2.6.1";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "0zmvagzisnk7lj5pfipl19mjq9wn70i339hpbkfljf0vk6s9fk2j";
			};

			beamDeps = [];
		};

		joken = buildMix rec {
			name = "joken";
			version = "2.4.0";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "0clxp6xkb7xyhhn3v2sviar9py686ygj3ad6fw6bwj757jch96ar";
			};

			beamDeps = [ jose ];
		};

		earmark_parser = buildMix rec {
			name = "earmark_parser";
			version = "1.4.15";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "0p6a442517w79j7q8fvyqb9n31h03cifqxy8iqdrr8cf8gb26i84";
			};

			beamDeps = [];
		};

		distillery = buildMix rec {
			name = "distillery";
			version = "2.1.1";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "1599wan14amzzkw93a9ikk6gql934frva0yrv0qg39k1065h1ixv";
			};

			beamDeps = [ artificery ];
		};

		file_system = buildMix rec {
			name = "file_system";
			version = "0.2.10";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "1p0myxmnjjds8bbg69dd6fvhk8q3n7lb78zd4qvmjajnzgdmw6a1";
			};

			beamDeps = [];
		};

		connection = buildMix rec {
			name = "connection";
			version = "1.1.0";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "1746n8ba11amp1xhwzp38yfii2h051za8ndxlwdykyqqljq1wb3j";
			};

			beamDeps = [];
		};

		idna = buildRebar3 rec {
			name = "idna";
			version = "6.1.1";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "1sjcjibl34sprpf1dgdmzfww24xlyy34lpj7mhcys4j4i6vnwdwj";
			};

			beamDeps = [ unicode_util_compat ];
		};

		dialyxir = buildMix rec {
			name = "dialyxir";
			version = "1.1.0";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "16hbdhkbqq69v452vx3c0d26mmadkmwr6nvdpr72c5azqi4qxsh7";
			};

			beamDeps = [ erlex ];
		};

		phoenix_live_reload = buildMix rec {
			name = "phoenix_live_reload";
			version = "1.3.3";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "1b5blinpmzdgspgk0dsy01bfjwwnhikb1gfiwnx8smazdrkrcrvn";
			};

			beamDeps = [ file_system phoenix ];
		};

		plug = buildMix rec {
			name = "plug";
			version = "1.12.1";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "02ykw1666amp8mjzfwgm155fp4fszg2nv5l4nya09hkvfyd7jznm";
			};

			beamDeps = [ mime plug_crypto telemetry ];
		};

		phoenix_ecto = buildMix rec {
			name = "phoenix_ecto";
			version = "4.4.0";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "1h9wnjmxns8y8dsr0r41ks66gscaqm7ivk4gsh5y07nkiralx1h9";
			};

			beamDeps = [ ecto phoenix_html plug ];
		};

		ecto_sql = buildMix rec {
			name = "ecto_sql";
			version = "3.7.0";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "0w0gqvmlyyh7avxfwqg79xd579mv4px4qilcj9xgi6yrl7gkaqd2";
			};

			beamDeps = [ db_connection ecto myxql postgrex telemetry ];
		};

		db_connection = buildMix rec {
			name = "db_connection";
			version = "2.4.0";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "1j6psw0dxq1175b6zcqpm6vavv4n6sv72ji57l8b6qczmlhnqhdd";
			};

			beamDeps = [ connection telemetry ];
		};

		plug_crypto = buildMix rec {
			name = "plug_crypto";
			version = "1.2.2";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "1nxnxj62iv4yvm4771jbxpj3l4brn2crz053y12s998lv5x1qqw7";
			};

			beamDeps = [];
		};

		ex_parameterized = buildMix rec {
			name = "ex_parameterized";
			version = "1.3.7";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "0p329aswjq95lwmn1iglsffgr855ck6kpl0673i2mhg8m55drc0z";
			};

			beamDeps = [];
		};

		ex_doc = buildMix rec {
			name = "ex_doc";
			version = "0.25.3";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "1x8kg59c3rhvjrn6fjizm4y9n669302gsyg7x4w2lwzcd4hvrgly";
			};

			beamDeps = [ earmark_parser makeup_elixir makeup_erlang ];
		};

		confex = buildMix rec {
			name = "confex";
			version = "3.5.0";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "0w1mav40n72hix4lpy6ald5y8kn8s10h7c0dwvdv7yy7609f7a9l";
			};

			beamDeps = [];
		};

		phoenix = buildMix rec {
			name = "phoenix";
			version = "1.5.12";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "04s16a1hjlwjkdkq4gz4xacvhvy4xcpiw5kccjmbn66c9xryc2lg";
			};

			beamDeps = [ jason phoenix_html phoenix_pubsub plug plug_cowboy plug_crypto telemetry ];
		};

		postgrex = buildMix rec {
			name = "postgrex";
			version = "0.15.10";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "1gijc67mnxi80vsxgsw4rpdircx3w4x670g2z09v5xj2fm1clq0m";
			};

			beamDeps = [ connection db_connection decimal jason ];
		};

		parse_trans = buildRebar3 rec {
			name = "parse_trans";
			version = "3.3.1";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "12w8ai6b5s6b4hnvkav7hwxd846zdd74r32f84nkcmjzi1vrbk87";
			};

			beamDeps = [];
		};

		makeup_erlang = buildMix rec {
			name = "makeup_erlang";
			version = "0.1.1";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "1fvw0zr7vqd94vlj62xbqh0yrih1f7wwnmlj62rz0klax44hhk8p";
			};

			beamDeps = [ makeup ];
		};

		edeliver = buildMix rec {
			name = "edeliver";
			version = "1.8.0";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "188q70zi9fm3z74z8hd7qlfc3l43ny3byl0ff23bhxs0jbnrpvq2";
			};

			beamDeps = [ distillery ];
		};

		artificery = buildMix rec {
			name = "artificery";
			version = "0.4.3";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "0105zjghn01zncvwza1bkih0apkz7vxbxgbsjd78h80flcrm7s8j";
			};

			beamDeps = [];
		};

		toml = buildMix rec {
			name = "toml";
			version = "0.6.2";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "1ykjlsjiq1w6kbmhyisjqygs2gmqx7lgaccdlck0qk6p4r8y84yh";
			};

			beamDeps = [];
		};

		mime = buildMix rec {
			name = "mime";
			version = "1.6.0";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "19qrpnmaf3w8bblvkv6z5g82hzd10rhc7bqxvqyi88c37xhsi89i";
			};

			beamDeps = [];
		};

		jose = buildRebar3 rec {
			name = "jose";
			version = "1.11.2";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "1lj715gzl022yc47qsg9712x8nc9wi7x70msv8c3lpym92y3y54q";
			};

			beamDeps = [];
		};

		phoenix_pubsub = buildMix rec {
			name = "phoenix_pubsub";
			version = "2.0.0";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "0wgpa19l6xar0k2m117iz2kq3cw433llp07sqswpf5969y698bf5";
			};

			beamDeps = [];
		};

		gettext = buildMix rec {
			name = "gettext";
			version = "0.18.2";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "1igmn69xzj5wpkblg3k9v7wa2fjc2j0cncwx0grk1pag7nqkgxgr";
			};

			beamDeps = [];
		};

		oauth2 = buildMix rec {
			name = "oauth2";
			version = "2.0.0";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "1rz7j456fi5qmakilfi36zd82w1zpsf3fjbrvkyzk1bkmij866w8";
			};

			beamDeps = [ hackney ];
		};

		makeup_elixir = buildMix rec {
			name = "makeup_elixir";
			version = "0.15.1";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "1j3fdjp915nq44c55gwysscryyqivjnaaign0wman1sb4drw2s6v";
			};

			beamDeps = [ makeup nimble_parsec ];
		};

		telemetry = buildRebar3 rec {
			name = "telemetry";
			version = "0.4.3";

			src = fetchHex {
				pkg = "${name}";
				version = "${version}";
				sha256 = "0hc0fr2bh97wah9ycpm7hb5jdqr5hnl1s3b2ibbbx9gxbwvbhwpb";
			};

			beamDeps = [];
		};
	};
in self

