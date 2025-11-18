# GNU C++ Compiler
CXX = g++
# C++ standard and flags
CXXFLAGS = -std=c++17 -Wall -Wextra -g
# Linker flags for AddressSanitizer
LDFLAGS = -fsanitize=address
# Add AddressSanitizer flags to compile flags as well
CXXFLAGS += -fsanitize=address

# Executable name
TARGET = main

# Find all .cpp files in the current directory
SOURCES = $(wildcard *.cpp)
# Create a list of .o files from the .cpp files
OBJECTS = $(SOURCES:.cpp=.o)

# Default target: build the executable
all: $(TARGET)

# Link the executable
$(TARGET): $(OBJECTS)
	$(CXX) $(LDFLAGS) -o $(TARGET) $(OBJECTS)

# Compile .cpp files to .o files
# This rule handles all .cpp files
%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Clean up build files
clean:
	rm -f $(TARGET) $(OBJECTS)

.PHONY: all clean