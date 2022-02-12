terraform {
  backend "gcs" {
    bucket = "zjn-sandbox-tf-state"
  }

  required_providers {
    namecheap = {
      source  = "namecheap/namecheap"
      version = ">= 2.0.0"
    }
  }
}

provider "google" {
  project = "zjn-sandbox"
  region  = "us-west1"
}

provider "namecheap" {
  user_name = "znewman01"
  api_user  = "znewman01"
}

resource "google_compute_region_disk" "backups" {
  name          = "zjn-cloud-backups"
  type          = "pd-ssd"
  size          = 30
  replica_zones = ["us-west1-a", "us-west1-c"]
}

resource "google_compute_firewall" "default" {
  name    = "zjn-cloud-firewall"
  network = "default"

  allow {
    protocol = "tcp"
    ports    = ["80", "443"]
  }

  source_ranges = ["0.0.0.0/0"]
}

resource "google_compute_instance" "default" {
  name         = "zjn-cloud"
  machine_type = "e2-small"
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

  lifecycle { ignore_changes = [attached_disk] }
}

resource "google_compute_attached_disk" "backups" {
  disk        = google_compute_region_disk.backups.id
  instance    = google_compute_instance.default.id
  device_name = "backups"
}

resource "namecheap_domain_records" "znewman-net" {
  domain = "znewman.net"

  record {
    hostname = "files"
    type     = "A"
    address  = google_compute_instance.default.network_interface[0].access_config[0].nat_ip
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