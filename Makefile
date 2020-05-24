CXX ?= g++

SRC_PATH = src
BUILD_PATH = build
BIN_PATH = $(BUILD_PATH)/bin
BIN_NAME = clam
SRC_EXT = cpp

PARSER_PATH = $(SRC_PATH)/ast/parser
PARSER_OUT_PATH = $(BUILD_PATH)/parser
BISONF = parser
FLEXF = lexer

SOURCES = $(shell find $(SRC_PATH) -name '*.$(SRC_EXT)' | sort -k 1nr | cut -f2-)
OBJECTS = $(SOURCES:$(SRC_PATH)/%.$(SRC_EXT)=$(BUILD_PATH)/%.o)
DEPS = $(OBJECTS:.o=.d)

CXXFLAGS = -std=c++17 -Wall -Wextra -g
LFLAGS = -lpthread
INCLUDES = -I include -I build/parser -I /usr/local/include

TEST_ARGS = example/example.clm

.PHONY: default_target
default_target: all

.PHONY: dirs
dirs:
	@echo "Creating directories"
	@mkdir -p $(dir $(OBJECTS))
	@mkdir -p $(BIN_PATH)
	@mkdir -p $(PARSER_OUT_PATH)

.PHONY: clean
clean:
	@echo "Deleting $(BIN_NAME) symlink"
	@$(RM) $(BIN_NAME)
	@echo "Deleting directories"
	@$(RM) -r $(BUILD_PATH)
	@$(RM) -r $(BIN_PATH)

.PHONY: all
all: $(BIN_PATH)/$(BIN_NAME)
	@echo "Making symlink: $(BIN_NAME) -> $<"
	@$(RM) $(BIN_NAME)
	@ln -s $(BIN_PATH)/$(BIN_NAME) $(BIN_NAME)

.PHONY: test
test: all
	@echo "Running Tests"
	./$(BIN_NAME) $(TEST_ARGS)

.PHONY: debug
debug: all
	@echo "DEBUGGING"
	gdb --args ./$(BIN_NAME) $(TEST_ARGS)

$(BIN_PATH)/$(BIN_NAME): $(OBJECTS) $(BUILD_PATH)/$(FLEXF).o $(BUILD_PATH)/$(BISONF).o
	@echo "Linking: $@"
	$(CXX) $(OBJECTS) $(BUILD_PATH)/$(FLEXF).o $(BUILD_PATH)/$(BISONF).o -o $@ $(LFLAGS)

.PHONY: parser_src
parser_src: $(PARSER_OUT_PATH)/$(FLEXF).cpp $(PARSER_OUT_PATH)/$(BISONF).cpp

$(PARSER_OUT_PATH)/$(FLEXF).cpp: $(PARSER_PATH)/$(FLEXF).l
	flex $<

$(PARSER_OUT_PATH)/$(BISONF).cpp: $(PARSER_PATH)/$(BISONF).y
	bison $< 

-include $(DEPS)

$(BUILD_PATH)/%.o: $(SRC_PATH)/%.$(SRC_EXT) dirs parser_src
	@echo "Compiling: $< -> $@"
	$(CXX) $(CXXFLAGS) $(INCLUDES) -MP -MMD -c $< -o $@

$(BUILD_PATH)/$(FLEXF).o: $(PARSER_OUT_PATH)/$(FLEXF).cpp $(PARSER_OUT_PATH)/$(BISONF).cpp dirs
	@echo "Compiling: $< -> $@"
	$(CXX) $(CXXFLAGS) $(INCLUDES) -I $(PARSER_OUT_PATH) -MP -MMD -c $< -o $@

$(BUILD_PATH)/$(BISONF).o: $(PARSER_OUT_PATH)/$(BISONF).cpp dirs
	@echo "Compiling: $< -> $@"
	$(CXX) $(CXXFLAGS) $(INCLUDES) -MP -MMD -c $< -o $@

