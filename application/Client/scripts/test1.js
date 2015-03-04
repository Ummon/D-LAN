dlan.print('Test core connection');

var core = dlan.newConnection()

core.connected.connect(function(){
   dlan.print("Connected!")
   core.sendChatMessage("This is only a test")
   core.disconnectFromCore()
})

core.connectToCore()

dlan.print("Script end")

/*
var basPort = 59490
var cors = []
for (int i = 0; i < 100; i++)
{
   cors[port] = dlan.newConnection(basPort + i)

   core.connected.connect(function(){
      print("Connected!")   
      for (var i = 0; i < 10; i++)
         core.sendChatMessage("Hi, i equals " + i)
      core.disconnectFromCore()
   })

   core.connectToCore()
}*/
