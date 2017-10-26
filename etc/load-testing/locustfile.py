from locust import HttpLocust, task, TaskSet
from random import randrange

class HoogleQueries(TaskSet):
    @task
    def hoogle_queries(self):
        # TODO: Get actual common queries from server logs
        _hoogle_queries = [
            "Ord",
            "Eq",
            "Num",
            "pack",
            "Text -> String",
            "fmap",
            "a -> a",
            "traverse",
            "bracket",
            "^.",
            ">>>",
            "<$>",
            "bimap",
            "inject"
        ]
        for q in _hoogle_queries:
            self.client.get("/lts/hoogle?q=" + q, name="/lts/hoogle?q=[:query]")

    @task
    def stop(self):
        self.interrupt()

class PackageBrowser(TaskSet):
    @task(50)
    def list_packages(self):
        self.client.get("/lts")

    @task(10)
    def browse_package(self):
        # TODO: Get packages to test from up-to-date listing on Stackage, move out of here
        _packages = [
            "accelerate",
            "adjunctions",
            "aeson",
            "binary",
            "both",
            "extensible-effects",
            "hamlet",
            "hdocs",
            "microlens",
            "range",
            "sort",
            "text",
            "universe"
        ]
        self.client.get("/lts/package/" + _packages[randrange(len(_packages))], name="/lts/package/:package")

    @task(2)
    def stop(self):
        self.interrupt()

class TopLevelPages(TaskSet):
    @task(30)
    def docs(self):
        self.client.get("/docs")

    @task(20)
    def install(self):
        self.client.get("/install")

    @task(5)
    def nightly(self):
        self.client.get("/nightly")

    @task(2)
    def snapshots(self):
        self.client.get("/snapshots")

    @task(2)
    def stop(self):
        self.interrupt()

class UserBehaviour(TaskSet):
    tasks = {
        HoogleQueries  : 2,
        PackageBrowser : 2,
        TopLevelPages  : 1
    }

class WebsiteUser(HttpLocust):
    task_set = UserBehaviour
    min_wait = 1000
    max_wait = 9000
