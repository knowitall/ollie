FROM maven:3.5.2-jdk-7

WORKDIR /stage

COPY ./ /stage/
RUN curl http://www.maltparser.org/mco/english_parser/engmalt.linear-1.7.mco > /stage/engmalt.linear-1.7.mco
RUN mvn clean package

CMD ["java", "-Xmx512m", "-jar", "ollie-app-1.0.1-SNAPSHOT.jar"]
