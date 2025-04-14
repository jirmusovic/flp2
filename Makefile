PROLOG = swipl
SRC = main.pl
TARGET = flp24-log

all: $(TARGET)

$(TARGET): $(SRC)
	$(PROLOG) -q -g start -o $(TARGET) -c $(SRC)

clean:
	rm -f $(TARGET)