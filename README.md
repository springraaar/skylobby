# skylobby [![license](https://img.shields.io/github/license/skynet-gh/skylobby)](LICENSE)

A Multiplayer Lobby Client for servers which use [uberserver's](https://github.com/spring/uberserver) protocol and host games which run on the [Spring RTS](https://springrts.com/) or [Recoil](https://beyond-all-reason.github.io/spring/) game engines. It'll let users register and stay connected to multiple servers simultaneously, join battle rooms and will automatically download the required game, map and engine packages. After setting up and downloading the requirements, users can play in the hosted rooms, set up their own or start local single player battles.

This is a fork from the original [Skylobby project](https://github.com/skynet-gh/skylobby/), starting from release 0.9.31. Releases will stick to the a.b.cXXX convention where the a.b.c prefix matches the latest release it's based off of from the original repository and the XXX is the local release number (done for compatibility with the [windows versioning constraints](https://stackoverflow.com/a/52414691) and the installer generation workflow).

Although it was tweaked with the Metal Factions community in mind, it's still meant to be a generic lobby client for various games. Key changes:
- Adds the Metal Factions server to the default server list
- Changes the tracked repository for auto-updates to this one
- Has a few UI improvements and changes to default settings
(check the release notes or change log for details)

Users can change to this or revert back to the original skylobby by running the corresponding installer regardless of the existing installation (previous settings are kept, though, so new defaults may not apply).

## Usage

The recommended way to install skylobby is to run the installer package for the operating system:
- [Windows](https://github.com/springraaar/skylobby/releases/download/0.9.31009/skylobby-0.9.31009_windows.msi)
- [Ubuntu/Debian](https://github.com/springraaar/skylobby/releases/download/0.9.31009/skylobby-0.9.31009_linux-amd64.deb)
- [Fedora/SUSE](https://github.com/springraaar/skylobby/releases/download/0.9.31009/skylobby-0.9.31009_linux-amd64.rpm)

The original install instructions and basic usage can be found in the [User Guide](https://github.com/skynet-gh/skylobby/wiki/User-Guide).

Feel free to open an [issue](https://github.com/springraaar/skylobby/issues) if you find a bug or have a feature request.

## Dev

Below are instructions on how to build and contribute to skylobby.

You will need [Java 11 or higher](https://adoptium.net/) and the [Clojure CLI tools](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools).

### REPL

Runs an interactive compiler as well as the UI. If you make a change to something in `src/clj` it will trigger a recompile and re-render the UI from the running state.

You will also need to install `rlwrap`. Run:

```bash
clj -M:nrepl
```

Logs are written to `repl.log`.

### Jar

To build and run executable jar files, it's advisable to use Java 21 or later. Current installer packages use Java SE builds based on OpenJDK from [Adoptium](https://adoptium.net/temurin/releases/), version 21.0.4.

Building the jar file:

```bash
clojure -M:uberjar
```
NOTE: you can also download the latest .jar file from the [releases page](https://github.com/springraaar/skylobby/releases).

Running the jar file:

```bash
java -jar target/skylobby.jar
```
The previous command lets the JVM grab way more RAM than the application needs, to get a smaller memory footprint, try
```bash
java -jar -XX:+ExitOnOutOfMemoryError -XX:MaxRAM=2g -XX:MaxRAMPercentage=80 -XX:+UseG1GC -XX:+UseCompressedOops -XX:G1PeriodicGCSystemLoadThreshold=0 -XX:-G1PeriodicGCInvokesConcurrent -XX:G1PeriodicGCInterval=30000 -XX:MaxHeapFreeRatio=8 -XX:MinHeapFreeRatio=4 target/skylobby.jar
```

To build an installer, then run `jpackage` for your platform, for example on Windows

```bash
jpackage @jpackage/lobby @jpackage/windows
```
