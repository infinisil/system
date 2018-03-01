{ eclipses }:

with eclipses;

eclipseWithPlugins {
  eclipse = eclipse-platform;
  plugins = with plugins; [
    jdt
    vrapper
  ];
}
