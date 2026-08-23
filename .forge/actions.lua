local publish = [[set -euo pipefail; python3 .forge/publish]]

return {
    publish = function(_)
        return {
            base = publish,
            packages = {
                publish = {
                    { registry = "41021897", ecosystem = "nuget", mode = "explicit", prefix = "FSharpGit" },
                },
            },
        }
    end,
}
