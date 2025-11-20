# GNU C++ Compiler
CXX = g++
# C++ standard and flags
CXXFLAGS = -std=c++17 -Wall -Wextra -g

# Executable name
TARGET = codegen

# Find all .cpp files in the current directory
SOURCES = $(wildcard *.cpp)
# Create a list of .o files from the .cpp files
OBJECTS = $(SOURCES:.cpp=.o)

# Default target: build the executable
all: $(TARGET)

# Link the executable
$(TARGET): $(OBJECTS)
	$(CXX) -o $(TARGET) $(OBJECTS)

# Compile .cpp files to .o files
# This rule handles all .cpp files
%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Clean up build files
clean:
	rm -f $(TARGET) $(OBJECTS)

.PHONY: all clean