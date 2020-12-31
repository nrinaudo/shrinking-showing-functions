enablePlugins(TutPlugin, GhpagesPlugin)

name         := "Shrinking and showing functions"
organization := "com.nrinaudo"
scalaVersion := "2.13.0"

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.15.1",
  "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.5"
)


val tutDirName = settingKey[String]("tut output directory")
tutDirName := "./"

val graphvizDirName = settingKey[String]("graphviz output directory")
graphvizDirName := "./img"

val graphviz = taskKey[Seq[(File, String)]]("compile all dot files")
graphviz := {
  import scala.sys.process._

  val files = (sourceDirectory.value / "graphviz") ** "*.dot"
  val outDir = target.value / "graphviz"

  outDir.mkdirs

  files.get.map { file =>
    val outFile = file.getName().replace(".dot", ".svg")
    val outPath = outDir / outFile

    s"dot -Tsvg $file -o $outPath" !

    (outPath, outFile)
  }
}

addMappingsToSiteDir(tut, tutDirName)
addMappingsToSiteDir(graphviz, graphvizDirName)
includeFilter in SitePlugin.autoImport.makeSite :=
    "*.yml" | "*.md" | "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.eot" | "*.svg" | "*.ttf" |
    "*.woff" | "*.woff2" | "*.otf"

git.remoteRepo := "git@github.com:nrinaudo/shrinking-showing-functions.git"
