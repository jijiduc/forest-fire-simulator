# Installation Requirements

## Prerequisites

### Required Software
- **Java 17+** (OpenJDK or Oracle JDK) - *Tested with Java 24.0.1*
- **sbt 1.10+** (Scala Build Tool) - *Tested with sbt 1.10.11*
- **Scala 3.7.0** (handled by sbt)

### Installation Commands

#### Ubuntu/Debian
```bash
# Install Java
sudo apt update
sudo apt install openjdk-17-jdk

# Install sbt
echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | sudo tee /etc/apt/sources.list.d/sbt.list
curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo apt-key add
sudo apt update
sudo apt install sbt
```

#### macOS
```bash
# Using Homebrew
brew install openjdk@17 sbt
```

#### Arch Linux
```bash
sudo pacman -S jdk17-openjdk sbt
```

## Project Setup

1. Clone the repository
2. Navigate to project directory
3. Run setup commands:

```bash
# Compile project
sbt compile

# Run tests
sbt test

# Run application
sbt run
```

## Dependencies

All Scala dependencies are managed automatically by sbt:
- Scala 3.7.0
- ScalaTest 3.2.19 (testing)
- Scalactic 3.2.19 (testing utilities)

## Verification

Test your setup:
```bash
sbt run
```

Should output: "Hello, World!" and "Forest Fire Simulator environment is working!"