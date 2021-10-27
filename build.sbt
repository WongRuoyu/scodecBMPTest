name := "scodecBMPTest project"

version := "1.0.0-SNAPSHOT"

organization := "com.wong"

scalaVersion := "2.12.3"

resolvers ++= Seq(
  "snapshots"     at "https://oss.sonatype.org/content/repositories/snapshots",
  "staging"       at "https://oss.sonatype.org/content/repositories/staging",
  "releases"      at "https://oss.sonatype.org/content/repositories/releases"
)

enablePlugins(JettyPlugin)

//unmanagedResourceDirectories in Test <+= (baseDirectory) { _ / "src/main/webapp" }

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

libraryDependencies ++= {
  val liftVersion = "3.0.1"
  Seq(
    "net.liftweb"       %% "lift-webkit"        % liftVersion        % "compile",
    "net.liftweb"       %% "lift-mapper"        % liftVersion        % "compile",
    "net.liftmodules"   %% "fobo_3.0"           % "1.7"              % "compile",
    "com.chuusai" %% "shapeless" % "2.3.7",
    "org.scalanlp" %% "breeze" % "1.1",
    "org.typelevel" %% "cats-effect" % "2.5.1",
    "org.scodec" %% "scodec-stream" % "2.0.2",
    "org.scodec" %% "scodec-protocols" % "2.0.0",
    "org.scodec" %% "scodec-cats" % "1.1.0",
    "co.fs2" %% "fs2-scodec" % "1.0.0-M1",
    "co.fs2" %% "fs2-core" % "2.5.6",
    "co.fs2" %% "fs2-io" % "2.5.6",
    "ch.qos.logback"    % "logback-classic"     % "1.1.3",
    "org.specs2"        %% "specs2-core"        % "3.9.4"              % "test",
    "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" % "container,test" artifacts Artifact("javax.servlet", "jar", "jar"),
    "ch.qos.logback"    % "logback-classic"     % "1.2.3",
    "com.h2database"    % "h2"                  % "1.4.187",
    "javax.servlet"     % "javax.servlet-api"   % "3.0.1"            % "provided"
  )
}
