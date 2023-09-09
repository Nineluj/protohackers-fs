FROM mcr.microsoft.com/dotnet/sdk:7.0 as build-env

WORKDIR /src
COPY *.fsproj .
COPY **.fs .
RUN dotnet restore
RUN dotnet publish -c Release -o /publish

FROM mcr.microsoft.com/dotnet/aspnet:7.0 as runtime

WORKDIR /publish
COPY --from=build-env /publish .
EXPOSE 10000
CMD dotnet $(ls | grep \.runtimeconfig\.json | sed 's/.runtimeconfig.json//').dll
