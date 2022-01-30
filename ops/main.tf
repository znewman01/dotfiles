terraform {
  backend "gcs" {
    bucket = "zjn-sandbox-tf-state"
  }
}

provider "google" {
  project = "zjn-sandbox"
  region  = "us-west1"
}

resource "google_compute_instance" "default" {
  name         = "zjn-cloud"
  machine_type = "e2-micro"
  zone         = "us-west1-a"

  boot_disk {
    initialize_params {
      image = "debian-cloud/debian-9"
    }
  }

  network_interface {
    network = "default"
    access_config {
    }
  }


  metadata = {
    ssh-keys           = "root:${file("~/.ssh/id_ed25519.pub")}"
    serial-port-enable = "true"
    startup-script     = <<-EOT
      curl https://raw.githubusercontent.com/elitak/nixos-infect/36e19e3b306abf70df6f4a6580b226b6a11a85f9/nixos-infect | NIX_CHANNEL=nixos-21.11 bash -x
    EOT
  }
}

output "ip" { value = google_compute_instance.default.network_interface[0].access_config[0].nat_ip }

# resource "google_compute_firewall" "default" {
#  name    = "flask-app-firewall"
#  network = "default"
#
#  allow {
#    protocol = "tcp"
#    ports    = ["5000"]
#  }
#  source_tags = ["mynetwork"]
# }
