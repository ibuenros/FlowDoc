name := "flowdoc"

version := "0.1"

scalaVersion := "2.10.0"

organization := "buenrostro"

unmanagedSourceDirectories in Compile <<= Seq(baseDirectory(_ / "src" )).join

unmanagedSourceDirectories in Test <<= Seq(baseDirectory(_ / "test" )).join
