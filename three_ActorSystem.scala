package com.tAS.test


import akka.actor._
import scala.xml.Node
import java.io._

case object StartMessage
case object StopMessage
case object next

case class Link(l: String)
case class Content(con: String)

class Actor1(actor2: ActorRef) extends Actor {				//Link generator
  var num=2000000
  def receive = {
    
    case StartMessage =>
      self ! next
      
    case next =>
      num+=1
      if(num<=2000010)
      {
      val link = "http://www.imdb.com/title/tt"+num
      actor2 ! Link(link)
      self ! next
      }
      else
      {
        actor2 ! StopMessage
        context.stop(self)
      }
      
  }
}
 
class Actor2(actor3: ActorRef) extends Actor {			//Content fetcher
  
  def attributeEquals(name: String)(node: Node) = node.attribute(name).isDefined
  
  def getContent(link:String):String = {
		val parserFactory = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
		val parser = parserFactory.newSAXParser()
		val source = new org.xml.sax.InputSource(link)
		val NBFA = new scala.xml.parsing.NoBindingFactoryAdapter
		val res = NBFA.loadXML(source, parser)
		(res \\ "h1").text+" - "+(res \\ "p").filter(attributeEquals("itemprop")).text
	}
  
  def receive = {
    
    case Link(l) =>
      try{
      val content = getContent(l.toString)
      actor3 ! Content(content)
      }
      catch {
         case ex: FileNotFoundException =>{
            println("Missing file exception - Page not found")
         }
      }
      
    case StopMessage =>
      actor3 ! StopMessage
      context.stop(self)
      
  }
  
}
class Actor3 extends Actor { 										//Content writer
  
	def using[A <: {def close(): Unit}, B](param: A)(f: A => B): B = try { f(param) } finally { param.close() }
	
	def appendToFile(fileName:String, textData:String) =
	  using (new FileWriter(fileName, true)){
		fileWriter => using (new PrintWriter(fileWriter)) {
		  printWriter => printWriter.println(textData)
		}
	}
	
	var num = 1

	def receive = {
	  
	  case Content(con) =>
	    //outWriteContent(content.toString)
	    appendToFile("MovieDB.txt",num+" "+con.toString)
	    println("Done printing onto the file, job number: "+num)
	    num+=1
	    
	  case StopMessage =>
	    context.stop(self)
	
	}
}
 
object doJob extends App {
  
  val system = ActorSystem("3ActorSystem")
  val actor3 = system.actorOf(Props[Actor3], name = "actor3")
  val actor2 = system.actorOf(Props(new Actor2(actor3)), name = "actor2")
  val actor1 = system.actorOf(Props(new Actor1(actor2)), name = "actor1")
  //start the system
  actor1 ! StartMessage
  
}