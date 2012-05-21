print('Test core connection')

var file = new QFile()

file.filename = "test_script_1.js";
file.open(QFile.ReadOnly)
print(file.readAll())

var core = dlan.newConnection()

core.connected.connect(function(){
   print("Connected!")   
   /*for (var i = 0; i < 1; i++)
      core.sendChatMessage("Hi, i equals " + i)*/
   core.sendChatMessage("TAISTE")
   core.disconnectFromCore()
})

core.connectToCore()


/*
var basPort = 59490
var cors = []
for (int i = 0; i < 100; i++)
{
   cors[port] = dlan.newConnection("localhost", basPort + i)

   core.connected.connect(function(){
      print("Connected!")   
      for (var i = 0; i < 10; i++)
         core.sendChatMessage("Hi, i equals " + i)
      core.disconnectFromCore()
   })

   core.connectToCore()
}*/
