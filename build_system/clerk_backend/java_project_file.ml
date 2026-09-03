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

let format_target_jar_rule ppf (target_name, src_directory) =
  Format.fprintf ppf
    "<execution>@\n\
    \  <id>%s-jar</id>@\n\
    \  <phase>package</phase>@\n\
    \  <goals>@\n\
    \    <goal>jar</goal>@\n\
    \  </goals>@\n\
    \  <configuration>@\n\
    \    <finalName>%s</finalName>@\n\
    \    <includes>@\n\
    \      <include>%s/**</include>@\n\
    \    </includes>@\n\
    \  </configuration>@\n\
     </execution>"
    target_name target_name src_directory

let format_target_source_jar_rule ppf (target_name, src_directory) =
  Format.fprintf ppf
    "<execution>@\n\
    \  <id>%s-sources-jar</id>@\n\
    \  <phase>package</phase>@\n\
    \  <goals><goal>jar</goal></goals>@\n\
    \  <configuration>@\n\
    \    <attach>false</attach>@\n\
    \    <outputDirectory>.</outputDirectory>@\n\
    \    <finalName>%s</finalName>@\n\
    \    <includes>@\n\
    \      <include>%s/**</include>@\n\
    \    </includes>@\n\
    \  </configuration>@\n\
     </execution>"
    target_name target_name src_directory

let format_pom_xml ppf (targets : (string * string) list) =
  let open Format in
  fprintf ppf
    {|<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
                             https://maven.apache.org/xsd/maven-4.0.0.xsd">
  <modelVersion>4.0.0</modelVersion>
  <groupId>catala.project</groupId>
  <artifactId>catala-project</artifactId>
  <version>0</version>
  <properties>
    <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
    <maven.compiler.release>17</maven.compiler.release>
  </properties>
  <build>
    <sourceDirectory>.</sourceDirectory>
    <directory>_java_artifacts</directory>
    <plugins>
      <plugin>

        <groupId>org.apache.maven.plugins</groupId>
        <artifactId>maven-jar-plugin</artifactId>
        <version>2.3.1</version>
        <configuration>
          <outputDirectory>.</outputDirectory>
        </configuration>

        <executions>
          <execution>
            <id>default-jar</id>
            <phase>none</phase>
          </execution>

          <!-- Generate Catala runtime and stdlib jar -->
          <execution>
            <id>runtime-jar</id>
            <phase>package</phase>
            <goals>
              <goal>jar</goal>
            </goals>
            <configuration>
              <finalName>runtime</finalName>
              <includes>
                <include>catala/**</include>
                <include>org/**</include>
              </includes>
            </configuration>
          </execution>
@[<v 10>@ %a@]

        </executions>
      </plugin>

      <plugin>
        <groupId>org.apache.maven.plugins</groupId>
        <artifactId>maven-source-plugin</artifactId>
        <version>2.1</version>
        <executions>

          <!-- Generate Catala runtime and stdlib jar sources -->
          <execution>
            <id>runtime-sources-jar</id>
            <phase>package</phase>
            <goals><goal>jar</goal></goals>
            <configuration>
              <outputDirectory>.</outputDirectory>
              <finalName>runtime</finalName>
              <includes>
                <include>catala/**</include>
                <include>org/**</include>
              </includes>
            </configuration>
          </execution>
@[<v 10>@ %a@]

        </executions>
      </plugin>
    </plugins>
  </build>
</project>|}
    (pp_print_list
       ~pp_sep:(fun ppf () -> fprintf ppf "@ @ ")
       format_target_jar_rule)
    targets
    (pp_print_list
       ~pp_sep:(fun ppf () -> fprintf ppf "@ @ ")
       format_target_source_jar_rule)
    targets
