{ lib, ...}: {
  # For some reason this isn't needed anymore
  system.activationScripts.lidWakeDisable = lib.stringAfter [ "specialfs" ] ''
    # Disable lid wake ability, fixes laptop waking up a few seconds after suspending without actually doing anything
    cat /proc/acpi/wakeup | grep -q 'LID0.*enabled' && echo LID0 > /proc/acpi/wakeup
  '';

  # This might be a better alternative, test this if it ever happens again
  # Update: doesn't work
  #services.udev.extraRules = ''
  #  SUBSYSTEM=="platform", KERNEL=="PNP0C0D:00", ATTR{power/wakeup}="disabled"
  #'';
}
