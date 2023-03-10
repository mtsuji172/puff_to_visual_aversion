char command;
uint8_t LED = 1;
uint8_t LEDpin = 11;
int mydelay4puff = 500;

void setup(){
    pinMode(LEDpin, OUTPUT);
    digitalWrite(LEDpin, LOW);
    Serial.begin(19200);
}

void loop(){
    if(Serial.available()>0){
        command = Serial.read();
        if(command=='0'){ // LED ON with freq for LED (for duing visual stimulation)
            LED = 0;
        }
        if(command=='1'){ // LED OFF
            LED = 1;
        }
        if(command=='2'){ // LED ON with 1Hz (to mimic puffs)
            LED = 2;
        }
    }
    // LED ON during visual stimulation
    if(LED==0){
        digitalWrite(LEDpin, HIGH);
        delay(mydelay); //to be rewritten by singleobject_closed.py
        digitalWrite(LEDpin, LOW);
        delay(mydelay); //to be rewritten by singleobject_closed.py
    }

    // LED ON to mimic puffs
    if(LED==2){
        digitalWrite(LEDpin, HIGH);
        delay(mydelay4puff);
        digitalWrite(LEDpin, LOW);
        delay(mydelay4puff);
    }
}
