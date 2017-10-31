from locust import HttpLocust, task, TaskSet
from random import randrange

def random_element(xs):
    return xs[randrange(len(xs))]

def select_snapshot():
    _snapshots = [
        "lts",
        "nightly",
        "lts-9.10",
        "lts-9.7",
        "lts-9.6",
        "lts-9.5",
        "lts-8.8",
        "nightly-2017-07-05",
        "nightly-2017-05-30",
        "nightly-2017-03-25",
        "lts-7.20",
    ]
    return random_element(_snapshots)

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
        _snapshot = select_snapshot()
        for q in _hoogle_queries:
            self.client.get("/" + _snapshot + "/hoogle?q=" + q, name="/:snapshot/hoogle?q=[:query]")

    @task
    def stop(self):
        self.interrupt()

class Documentation(TaskSet):
    @task
    def docs(self):
        _snapshot = select_snapshot()
        self.client.get("/" + _snapshot + "/docs", name="/:snapshot/docs")

    @task
    def stop(self):
        self.interrupt()
        
class PackageBrowser(TaskSet):
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
        _snapshot = select_snapshot()
        self.client.get("/" + _snapshot + "/package/" + random_element(_packages), name="/:snapshot/package/:package")

    @task(2)
    def stop(self):
        self.interrupt()

class Snapshots(TaskSet):
    @task
    def updateSnapshots(self):
        self.client.get("/download/snapshots.json")
        
    @task
    def stop(self):
        self.interrupt()
       
class TopLevelPages(TaskSet):
    @task(20)
    def install(self):
        self.client.get("/install")

    @task(10)
    def lts(self):
        self.client.get("/lts")
        
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
        HoogleQueries  : 5,
        PackageBrowser : 2,
        Documentation  : 2,
        Snapshots      : 1,
        TopLevelPages  : 1,
    }

class WebsiteUser(HttpLocust):
    task_set = UserBehaviour
    min_wait = 1000
    max_wait = 9000
