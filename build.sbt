name := "FacebookAPI"

version       := "1.0"

scalaVersion  := "2.11.6"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= {
  val akkaV = "2.3.13"
  val sprayV = "1.3.3"
  Seq(
    "io.spray" 		  %%  "spray-json"    % "1.3.2",
    "io.spray"            %%  "spray-client"  % sprayV,	
    "io.spray"            %%  "spray-routing" % sprayV,
    "com.typesafe.akka"   %%  "akka-actor"    % akkaV,
    "com.typesafe.akka" %% "akka-slf4j" % "2.3.13",
    "ch.qos.logback" % "logback-classic" % "1.0.9",
    "commons-codec" % "commons-codec" % "1.7"

  )
}


