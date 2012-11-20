function pairs(t)
  local nextKey
  return
    function ()
      key, val = next(t, nextKey)
      if key
        then nextKey = key
      end
      return key, val
    end
end

function printtable(t)
  local k, v
  for k, v in pairs(t) do
    print(k, v)
  end
end

