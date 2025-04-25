import scala.io.Source
import java.io.PrintWriter
import scala.util.CommandLineParser.*
import java.util.Calendar
//run format: scala cryptography.scala -- <args>
object Crypt{
    def main(args:Array[String]) = {
    val len=args.length
    if(len>=2){
        val a0=args(0)
        val a1=args(1)
        if(a0.endsWith(".txt")&&a1.endsWith(".txt")){
                if(len>=3){
                    args(2) match{
                        case "offset" => 
                            if(len>=4) mapFile(a0,a1,offset(_,args(3).toInt))
                            else mapFile(a0,a1,offset(_,1)) 
                        case "flip" => mapFile(a0,a1,flip)
                        case "key" =>
                            var index = 0
                            if(len==3)
                                    mapFile(a0,a1,c=>{
                                    val offset=("default"(index)-'a')+26
                                    index=(index+1)%7
                                    if(c.isLower)('a'+(c-'a'+offset+26)%26).toChar
                                    else ('A'+(c-'A'+offset+26)%26).toChar
                                })
                            else {
                                val key=args(3)
                                if(len==4) mapFile(a0,a1,c=>{
                                    val offset=(key(index)-'a')+26
                                    index=(index+1)%key.length
                                    if(c.isLower)('a'+(c-'a'+offset+26)%26).toChar
                                    else ('A'+(c-'A'+offset+26)%26).toChar
                                })
                                else if(len>=5){
                                        val factor=args(4).toInt
                                        mapFile(a0,a1,c=>{
                                        val offset=(factor.toInt)*(key(index)-'a')+26
                                        index=(index+1)%key.length
                                        if(c.isLower)('a'+(c-'a'+offset+26)%26).toChar
                                        else ('A'+(c-'A'+offset+26)%26).toChar
                                    })
                                }
                            }
                        case "time" => mapFile(a0,a1,time)
                        case "kaizo" => mapFile(a0,a1,kaizo)
                        case "unoffset" => 
                            if(len>=4) mapFile(a0,a1,offset(_,args(3).toInt))
                            else mapFile(a0,a1,offset(_,1)) 
                        case "untime" => mapFile(a0,a1,unTime)
                    }
                } else mapFile(a0,a1,c=>c)
            } else println("Error: first two args must be text filenames")
        } else println("Error: must have two filename args")
    } 
}                

def mapFile(inFile:String,outFile:String,trans:Char=>Char) = {
    val pw = new PrintWriter(outFile)
    val in = Source.fromFile(inFile)
    for(c <- in){
        pw.print(if(c>='a'&&c<='z'||c>='A'&&c<='Z') trans(c) else c)
    }
    in.close
    pw.close
}

def offset(c:Char,o:Int):Char = {
    if(c.isLower) ('a'+(c-'a'+o+26)%26).toChar
    else ('A'+(c-'A'+o+26)%26).toChar
}
def flip(c:Char):Char = {
    if(c.isLower) ('a'+(25-(c-'a'))).toChar
    else ('A'+(25-(c-'A'))).toChar
}
def time(c:Char):Char = {
    val now = Calendar.getInstance()
    val currentMinute = now.get(Calendar.MINUTE)
    offset(c,currentMinute)
}
def kaizo(c:Char):Char = {
    val rand = new scala.util.Random
    flip(c)
    offset(c,rand.nextInt())
}

def unOffset(c:Char,o:Int):Char = {
    if(c.isLower) ('a'+(c-'a'-o+26)%26).toChar
    else ('A'+(c-'A'-o+26)%26).toChar
}
//flip undoes flip, dumbass
def unTime(c:Char):Char = {
    val now = Calendar.getInstance()
    val currentMinute = now.get(Calendar.MINUTE)
    unOffset(c,currentMinute)
}
