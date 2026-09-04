(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2020-2025 Inria,
   contributors: Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley
   <emile.rolley@tuta.io>, Louis Gesbert <louis.gesbert@inria.fr>,
   Vincent Botbol <vincent.botbol@inria.fr>.

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Catala_utils
open Clerk_utils

let format_dependency ~project_name ppf (dep_name : string) =
  let groupId, artifactId, version =
    if dep_name = Scan.libcatala then
      Scan.libcatala, "catala-runtime", Catala_utils.Version.v
    else project_name, String.to_snake_case dep_name, "1.0.0"
  in
  Format.fprintf ppf
    "    <dependency>\n\
    \      <groupId>%s</groupId>\n\
    \      <artifactId>%s</artifactId>\n\
    \      <version>%s</version>\n\
    \    </dependency>"
    groupId artifactId version

let format_target_pom_xml ~project_name ppf (target : Clerk_config.target) =
  let open Format in
  let groupId, artifactId, version, src_dir, finalName, include_dir =
    if target.tname = Scan.libcatala then
      ( Scan.libcatala,
        "catala-runtime",
        Catala_utils.Version.v,
        "${project.basedir}",
        "${project.artifactId}-${project.version}",
        "**/*.java" )
    else
      let dir_name = String.to_snake_case target.tname in
      ( project_name,
        dir_name,
        "1.0.0",
        "${project.basedir}/..",
        "${project.artifactId}",
        "${project.artifactId}/**/*.java" )
  in
  let pp_dependencies ppf =
    if target.dependencies = [] then ()
    else
      Format.fprintf ppf {|
  <dependencies>
%a
  </dependencies>

|}
        (pp_print_list ~pp_sep:Format.pp_print_newline
           (format_dependency ~project_name))
        target.dependencies
  in
  fprintf ppf
    {|<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="
                             http://maven.apache.org/POM/4.0.0
                             https://maven.apache.org/xsd/maven-4.0.0.xsd">

  <modelVersion>4.0.0</modelVersion>

  <groupId>%s</groupId>
  <artifactId>%s</artifactId>
  <version>%s</version>

  <properties>
    <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
    <maven.compiler.release>17</maven.compiler.release>
  </properties>
%t
  <build>
    <sourceDirectory>%s</sourceDirectory>
    <finalName>%s</finalName>
    <plugins>

      <plugin>
        <groupId>org.apache.maven.plugins</groupId>
        <artifactId>maven-compiler-plugin</artifactId>
        <version>3.14.0</version>
        <configuration>
          <release>${maven.compiler.release}</release>
          <includes>
            <include>%s</include>
          </includes>
        </configuration>
      </plugin>

      <plugin>
        <groupId>org.apache.maven.plugins</groupId>
        <artifactId>maven-source-plugin</artifactId>
        <version>3.4.0</version>
        <configuration>
          <includes>
            <include>%s</include>
          </includes>
          <includePom>false</includePom>
          <excludeResources>true</excludeResources>
        </configuration>
        <executions>
          <execution>
            <id>attach-sources</id>
            <phase>package</phase>
            <goals> <goal>jar-no-fork</goal> </goals>
          </execution>
        </executions>
      </plugin>

    </plugins>
  </build>

</project>|}
    groupId artifactId version pp_dependencies src_dir finalName include_dir
    include_dir

let format_project_pom_xml ~(config : Clerk_cli.config) ppf targets =
  let open Format in
  let project_name =
    Option.value ~default:"default-project" config.file.global.project_name
  in
  let format_module ppf (target : Clerk_config.target) =
    let mvn_mod_name =
      if target.tname = Scan.libcatala then target.tname
      else String.to_snake_case target.tname
    in
    fprintf ppf "<module>%s</module>" mvn_mod_name
  in
  let format_artifact_item ppf (target : Clerk_config.target) =
    let groupId, artifactId, version, destName =
      if target.tname = Scan.libcatala then
        ( Scan.libcatala,
          "catala-runtime",
          Version.v,
          "catala-runtime" ^ Version.v )
      else
        let dir_name = String.to_snake_case target.tname in
        project_name, dir_name, "${project.version}", dir_name
    in
    Format.fprintf ppf
      {|                  <artifactItem>
                    <groupId>%s</groupId>
                    <artifactId>%s</artifactId>
                    <version>%s</version>
                    <outputDirectory>${project.basedir}</outputDirectory>
                    <destFileName>%s.jar</destFileName>
                  </artifactItem>|}
      groupId artifactId version destName
  in
  let groupId, artifactId, version = project_name, "clerk-project", "1.0.0" in
  fprintf ppf
    {|<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="
             http://maven.apache.org/POM/4.0.0
             https://maven.apache.org/xsd/maven-4.0.0.xsd">

    <modelVersion>4.0.0</modelVersion>

    <groupId>%s</groupId>
    <artifactId>%s</artifactId>
    <version>%s</version>

    <packaging>pom</packaging>

    <modules>
%a
    </modules>

    <build>
      <plugins>
        <plugin>
          <groupId>org.apache.maven.plugins</groupId>
          <artifactId>maven-dependency-plugin</artifactId>
          <version>3.8.1</version>

          <executions>
            <execution>
              <id>copy-libraries</id>
              <phase>package</phase>
              <goals>
                <goal>copy</goal>
              </goals>
              <configuration>
                <artifactItems>
%a
                </artifactItems>
              </configuration>
            </execution>
          </executions>
        </plugin>
      </plugins>
    </build>

</project>|}
    groupId artifactId version
    (pp_print_list ~pp_sep:pp_print_newline format_module)
    targets
    (pp_print_list ~pp_sep:pp_print_newline format_artifact_item)
    targets
