local publish = [[set -euo pipefail; python3 .forge/publish]]

return {
    publish = function(_)
        return {
            base = publish,
            packages = {
                publish = {
                    { registry = "mega", ecosystem = "nuget", mode = "explicit", prefix = "FSharpGit" },
                },
            },
        }
    end,
}
