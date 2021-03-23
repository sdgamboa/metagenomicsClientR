# DefaultApi

All URIs are relative to *https://cmgd-telemetry-whnnxetv4q-uc.a.run.app*

Method | HTTP request | Description
------------- | ------------- | -------------
[**AddNextflowEventNextflowEventsPost**](DefaultApi.md#AddNextflowEventNextflowEventsPost) | **POST** /nextflow/events | Add Nextflow Event
[**CreateSampleMetadataSamplesPost**](DefaultApi.md#CreateSampleMetadataSamplesPost) | **POST** /metadata/samples | Create Sample
[**CreateStudyMetadataStudiesPost**](DefaultApi.md#CreateStudyMetadataStudiesPost) | **POST** /metadata/studies | Create Study
[**DeleteStudyMetadataStudiesUuidDelete**](DefaultApi.md#DeleteStudyMetadataStudiesUuidDelete) | **DELETE** /metadata/studies/{uuid} | Delete Study
[**FilesChangeFilesChangesPost**](DefaultApi.md#FilesChangeFilesChangesPost) | **POST** /files/changes | Files Change
[**GenerateDownloadSignedUrlV4FilesSignedGet**](DefaultApi.md#GenerateDownloadSignedUrlV4FilesSignedGet) | **GET** /files/signed | Generate Download Signed Url V4
[**GetNextflowEventNextflowEventsIdGet**](DefaultApi.md#GetNextflowEventNextflowEventsIdGet) | **GET** /nextflow/events/{id} | Get Nextflow Event
[**GetSamplesMetadataSamplesGet**](DefaultApi.md#GetSamplesMetadataSamplesGet) | **GET** /metadata/samples | Get Samples
[**GetStudiesMetadataStudiesGet**](DefaultApi.md#GetStudiesMetadataStudiesGet) | **GET** /metadata/studies | Get Studies
[**GetUuidFromStringMetadataUuidGet**](DefaultApi.md#GetUuidFromStringMetadataUuidGet) | **GET** /metadata/uuid | Get Uuid From String
[**ListFileEventsFilesChangesGet**](DefaultApi.md#ListFileEventsFilesChangesGet) | **GET** /files/changes | List File Events
[**ListNextflowEventsNextflowEventsGet**](DefaultApi.md#ListNextflowEventsNextflowEventsGet) | **GET** /nextflow/events | List Nextflow Events
[**SamplesForStudyMetadataStudiesUuidSamplesGet**](DefaultApi.md#SamplesForStudyMetadataStudiesUuidSamplesGet) | **GET** /metadata/studies/{uuid}/samples | Samples For Study
[**SamplesJsonMetadataSamplesjsonGet**](DefaultApi.md#SamplesJsonMetadataSamplesjsonGet) | **GET** /metadata/samplesjson | Samples Json


# **AddNextflowEventNextflowEventsPost**
> AnyType AddNextflowEventNextflowEventsPost(nf.model)

Add Nextflow Event

Events from running Nextflow pipelines when using -with-weblog.  See [the Nextflow documentation](https://www.nextflow.io/docs/latest/tracing.html#weblog-via-http).

### Example
```R
library(metagenomicsClientR)

var.nf.model <- NFModel$new("run_name_example", "run_id_example", "event_example", "utc_time_example", 123, 123) # NFModel | 

#Add Nextflow Event
api.instance <- DefaultApi$new()
result <- api.instance$AddNextflowEventNextflowEventsPost(var.nf.model)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **nf.model** | [**NFModel**](NFModel.md)|  | 

### Return type

[**AnyType**](AnyType.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful Response |  -  |
| **422** | Validation Error |  -  |

# **CreateSampleMetadataSamplesPost**
> AnyType CreateSampleMetadataSamplesPost(sample)

Create Sample

### Example
```R
library(metagenomicsClientR)

var.sample <- Sample$new("name_example", "hash_example") # Sample | 

#Create Sample
api.instance <- DefaultApi$new()
result <- api.instance$CreateSampleMetadataSamplesPost(var.sample)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **sample** | [**Sample**](Sample.md)|  | 

### Return type

[**AnyType**](AnyType.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful Response |  -  |
| **422** | Validation Error |  -  |

# **CreateStudyMetadataStudiesPost**
> AnyType CreateStudyMetadataStudiesPost(study)

Create Study

### Example
```R
library(metagenomicsClientR)

var.study <- Study$new("name_example", 123) # Study | 

#Create Study
api.instance <- DefaultApi$new()
result <- api.instance$CreateStudyMetadataStudiesPost(var.study)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **study** | [**Study**](Study.md)|  | 

### Return type

[**AnyType**](AnyType.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful Response |  -  |
| **422** | Validation Error |  -  |

# **DeleteStudyMetadataStudiesUuidDelete**
> AnyType DeleteStudyMetadataStudiesUuidDelete(uuid)

Delete Study

### Example
```R
library(metagenomicsClientR)

var.uuid <- 'uuid_example' # character | 

#Delete Study
api.instance <- DefaultApi$new()
result <- api.instance$DeleteStudyMetadataStudiesUuidDelete(var.uuid)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **uuid** | [**character**](.md)|  | 

### Return type

[**AnyType**](AnyType.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful Response |  -  |
| **422** | Validation Error |  -  |

# **FilesChangeFilesChangesPost**
> AnyType FilesChangeFilesChangesPost(body)

Files Change

Track all changes in cloud storage

### Example
```R
library(metagenomicsClientR)

var.body <- NULL # object | 

#Files Change
api.instance <- DefaultApi$new()
result <- api.instance$FilesChangeFilesChangesPost(var.body)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **object**|  | 

### Return type

[**AnyType**](AnyType.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful Response |  -  |
| **422** | Validation Error |  -  |

# **GenerateDownloadSignedUrlV4FilesSignedGet**
> AnyType GenerateDownloadSignedUrlV4FilesSignedGet(name)

Generate Download Signed Url V4

Generates a v4 signed URL for downloading a blob.  Note that this method requires a service account key file. You can not use this if you are using Application Default Credentials from Google Compute Engine or from the Google Cloud SDK.

### Example
```R
library(metagenomicsClientR)

var.name <- 'name_example' # character | 

#Generate Download Signed Url V4
api.instance <- DefaultApi$new()
result <- api.instance$GenerateDownloadSignedUrlV4FilesSignedGet(var.name)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **name** | **character**|  | 

### Return type

[**AnyType**](AnyType.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful Response |  -  |
| **422** | Validation Error |  -  |

# **GetNextflowEventNextflowEventsIdGet**
> AnyType GetNextflowEventNextflowEventsIdGet(id)

Get Nextflow Event

### Example
```R
library(metagenomicsClientR)

var.id <- 'id_example' # character | 

#Get Nextflow Event
api.instance <- DefaultApi$new()
result <- api.instance$GetNextflowEventNextflowEventsIdGet(var.id)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **id** | [**character**](.md)|  | 

### Return type

[**AnyType**](AnyType.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful Response |  -  |
| **422** | Validation Error |  -  |

# **GetSamplesMetadataSamplesGet**
> AnyType GetSamplesMetadataSamplesGet()

Get Samples

### Example
```R
library(metagenomicsClientR)


#Get Samples
api.instance <- DefaultApi$new()
result <- api.instance$GetSamplesMetadataSamplesGet()
dput(result)
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**AnyType**](AnyType.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful Response |  -  |

# **GetStudiesMetadataStudiesGet**
> AnyType GetStudiesMetadataStudiesGet()

Get Studies

### Example
```R
library(metagenomicsClientR)


#Get Studies
api.instance <- DefaultApi$new()
result <- api.instance$GetStudiesMetadataStudiesGet()
dput(result)
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**AnyType**](AnyType.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful Response |  -  |

# **GetUuidFromStringMetadataUuidGet**
> AnyType GetUuidFromStringMetadataUuidGet(string)

Get Uuid From String

### Example
```R
library(metagenomicsClientR)

var.string <- 'string_example' # character | 

#Get Uuid From String
api.instance <- DefaultApi$new()
result <- api.instance$GetUuidFromStringMetadataUuidGet(var.string)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **string** | **character**|  | 

### Return type

[**AnyType**](AnyType.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful Response |  -  |
| **422** | Validation Error |  -  |

# **ListFileEventsFilesChangesGet**
> StorageEventCollection ListFileEventsFilesChangesGet(limit=100, offset=0, start.date=var.start.date, end.date=var.end.date, filename.regex=var.filename.regex)

List File Events

### Example
```R
library(metagenomicsClientR)

var.limit <- 100 # integer | 
var.offset <- 0 # integer | 
var.start.date <- 'start.date_example' # character | 
var.end.date <- 'end.date_example' # character | 
var.filename.regex <- 'filename.regex_example' # character | 

#List File Events
api.instance <- DefaultApi$new()
result <- api.instance$ListFileEventsFilesChangesGet(limit=var.limit, offset=var.offset, start.date=var.start.date, end.date=var.end.date, filename.regex=var.filename.regex)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **limit** | **integer**|  | [optional] [default to 100]
 **offset** | **integer**|  | [optional] [default to 0]
 **start.date** | **character**|  | [optional] 
 **end.date** | **character**|  | [optional] 
 **filename.regex** | **character**|  | [optional] 

### Return type

[**StorageEventCollection**](StorageEventCollection.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful Response |  -  |
| **422** | Validation Error |  -  |

# **ListNextflowEventsNextflowEventsGet**
> NFCollection ListNextflowEventsNextflowEventsGet(limit=100, offset=0)

List Nextflow Events

### Example
```R
library(metagenomicsClientR)

var.limit <- 100 # integer | 
var.offset <- 0 # integer | 

#List Nextflow Events
api.instance <- DefaultApi$new()
result <- api.instance$ListNextflowEventsNextflowEventsGet(limit=var.limit, offset=var.offset)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **limit** | **integer**|  | [optional] [default to 100]
 **offset** | **integer**|  | [optional] [default to 0]

### Return type

[**NFCollection**](NFCollection.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful Response |  -  |
| **422** | Validation Error |  -  |

# **SamplesForStudyMetadataStudiesUuidSamplesGet**
> AnyType SamplesForStudyMetadataStudiesUuidSamplesGet(uuid)

Samples For Study

### Example
```R
library(metagenomicsClientR)

var.uuid <- 'uuid_example' # character | 

#Samples For Study
api.instance <- DefaultApi$new()
result <- api.instance$SamplesForStudyMetadataStudiesUuidSamplesGet(var.uuid)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **uuid** | [**character**](.md)|  | 

### Return type

[**AnyType**](AnyType.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful Response |  -  |
| **422** | Validation Error |  -  |

# **SamplesJsonMetadataSamplesjsonGet**
> AnyType SamplesJsonMetadataSamplesjsonGet()

Samples Json

### Example
```R
library(metagenomicsClientR)


#Samples Json
api.instance <- DefaultApi$new()
result <- api.instance$SamplesJsonMetadataSamplesjsonGet()
dput(result)
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**AnyType**](AnyType.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful Response |  -  |

