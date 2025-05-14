<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRdocumentsInformation`

## 1. Overview:

### Objective:
The `FRdocumentsInformation` unit is designed to manage and display document-related information in a grid format. It provides functionalities for adding, editing, and validating document data, such as document type, number of copies, email, fax, and address number. The grid is highly customizable, allowing for specific field configurations, validations, and user interactions.

### Technologies Used:
- **Delphi VCL Framework**: For creating the user interface and handling events.
- **SOAP Services**: For interacting with external services.
- **Database Components**: For managing and validating data.
- **cxGrid**: A component for displaying and managing tabular data.
- **cxEditRepository**: For custom field editors and validations.

### Form Type:
This is a **grid display**. 

#### Grid Columns and Their Types:
1. **tpDoc**: Document type (Image ComboBox).
2. **nCopies**: Number of copies (Mask Item).
3. **defMethod**: Default method (Image ComboBox).
4. **addressNum**: Address number (Mask Item).
5. **Email**: Email address (Mask Item).
6. **Fax**: Fax number (Mask Item).
7. **name**: Name (String).
8. **addText**: Additional text (String).
9. **lastUpd**: Last updated (String).
10. **updBy**: Updated by (String).

#### Grid Actions and Their Effects:
- **Add**: Adds a new record to the grid.
- **Delete**: Deletes the selected record from the grid.

---

## 2. Functionality Description:

### User/Software Actions:
1. Add a new document record.
2. Edit existing document records.
3. Validate email addresses.
4. Configure grid settings (e.g., hidden fields, field order, custom editors).

### Main Components:
- **Grid Settings**: Configures the grid's appearance and behavior.
- **Custom Editors**: Provides specific input types for fields (e.g., combo boxes, masks).
- **Validation Functions**: Ensures data integrity (e.g., email validation).

### Pseudo-code for Actions and Events:
- **On Add Button Click**:  
  `if add button clicked then add new record to dataset`
- **On Before Edit Event**:  
  `if dataset is about to be edited then perform pre-edit checks`
- **On Before Post Event**:  
  `if dataset is about to be posted then validate data`
- **Email Validation**:  
  `if email validation function called then check email format`

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The `TFRAMEdocumentsInformation` is created.
   - Grid settings are configured (e.g., hidden fields, field order, custom editors).
   - Default values and configurations are loaded.
2. **User Interaction**:
   - Users can add, edit, or delete records in the grid.
   - Validations are triggered during data entry or before saving.
3. **Functions**:
   - `Create`: Initializes the frame and configures grid settings.
   - `CDStableBeforeEdit`: Prepares the dataset for editing.
   - `CDStableBeforePost`: Validates data before saving.
   - `ACTaddExecute`: Adds a new record to the dataset.

### Required Data:
- Document type, number of copies, default method, address number, email, and fax.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Add Action**: Enabled at all times.
- **Delete Action**: Enabled only when a record is selected.
- **Save Action**: Enabled only when all required fields are valid.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- "Invalid email format" if the email validation fails.
- "Required field not completed" if a mandatory field is empty.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- **Email**: Must be a valid email format.
- **Fax**: Must match the regular expression `(\+\d{1,4})?\d{1,15}`.
- **Address Number**: Must be a numeric value (1-2 digits).
- **Number of Copies**: Must be a numeric value (1-2 digits).

---

## 5. Main Functions:

1. **`Create`**:
   - Initializes the frame and configures grid settings.
   - Sets default values and properties.
2. **`CDStableBeforeEdit`**:
   - Prepares the dataset for editing.
3. **`CDStableBeforePost`**:
   - Validates data before saving.
4. **`ACTaddExecute`**:
   - Adds a new record to the dataset.
5. **`m_ExistsRecord`**:
   - Checks if a record exists in the dataset.
6. **`ValidEmails`**:
   - Validates email addresses.

---

## 6. API Service Consumption:

- **Service Name**: Not explicitly defined in the code.
- **Endpoint**: Not explicitly defined in the code.
- **Data Sent**: Not explicitly defined in the code.
- **Data Received**: Not explicitly defined in the code.
- **Purpose**: Not explicitly defined in the code.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- **Address Number Field**: Always visible.
- **Conditions**: No conditional fields are defined in the code.

---

## 8. Dependencies:

### External Libraries:
- **cxGrid**: For grid display and management.
- **cxEditRepository**: For custom field editors.
- **SOAPHTTPClient**: For SOAP service interactions.

### Custom Components:
- **TFRAMEBaseGridEditSOA**: Base class for grid management.
- **kneFRGridManager**: For managing grid settings.

---

## 9. Fields and Validations Listing:

1. **tpDoc**:  
   - Type: Image ComboBox.  
   - Required: Yes.  
   - Values: `INV`, `OACK`, `PLIST`.

2. **nCopies**:  
   - Type: Mask Item.  
   - Required: Yes.  
   - Validation: Numeric (1-2 digits).

3. **defMethod**:  
   - Type: Image ComboBox.  
   - Required: Yes.  
   - Values: `MAIL`, `FAX`, `POSTAL`.

4. **addressNum**:  
   - Type: Mask Item.  
   - Required: Yes.  
   - Validation: Numeric (1-2 digits).

5. **Email**:  
   - Type: Mask Item.  
   - Required: Yes.  
   - Validation: Valid email format.

6. **Fax**:  
   - Type: Mask Item.  
   - Required: No.  
   - Validation: Matches regex `(\+\d{1,4})?\d{1,15}`.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```delphi
procedure TFRAMEdocumentsInformation.ACTaddExecute(Sender: TObject);
begin
  // Add a new record to the dataset
  CDStable.Append;
end;
```

### Screenshots:
Not applicable.

---

## 11. Important Comments in the Code:

- **Grid Settings**: Configures hidden fields, field order, and custom editors.
- **Validation Functions**: Ensures data integrity before saving.

---

## 12. Conclusion:

The `FRdocumentsInformation` unit provides a robust framework for managing document-related data in a grid format. It includes customizable grid settings, field validations, and user actions. However, the lack of explicit API integration and error handling limits its extensibility.

---

## 13. Short Summary:

The `FRdocumentsInformation` unit manages document data in a grid format, offering customizable settings, field validations, and user actions like adding and editing records. It is part of a larger system for document management.#### **FRdocumentsInformation.pas**

```
unit FRdocumentsInformation;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid,
  cxImageComboBox;

type
  TFRAMEdocumentsInformation = class(TFRAMEBaseGridEditSOA)
    cxICBdefMethd: TcxEditRepositoryImageComboBoxItem;
    cxEDTaddressNum: TcxEditRepositoryMaskItem;
    cxEDTFax: TcxEditRepositoryMaskItem;
    cxICBtypeDoc: TcxEditRepositoryImageComboBoxItem;
    cxEDTnCopies: TcxEditRepositoryMaskItem;
    cxEDTemail: TcxEditRepositoryMaskItem;
    procedure CDStableBeforeEdit(DataSet: TDataSet);
    procedure CDStableBeforePost(DataSet: TDataSet);
    procedure ACTaddExecute(Sender: TObject);

  private
    { Private declarations }
    FdefaultsLoaded: Boolean;
    function m_ExistsRecord(const pv_dataset: TClientDataSet;
      pv_fieldNames: string): Boolean;
    function ValidEmails(pv_Unique: Boolean = false): Boolean;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;
  end;

var
  FRAMEdocumentsInformation: TFRAMEdocumentsInformation;

implementation

uses
  kneUtils, kneTypes, kneFGDBUtils, Global, kneFREditSOA;

{$R *.dfm}


constructor TFRAMEdocumentsInformation.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  if Tform(owner).ClassName = 'TFORMMconsignee' then
    MasterKeyFields := 'consCode=entity;entityType'
  else
  if Tform(owner).ClassName = 'TFORMEcustomerAddressDoc' then
    MasterKeyFields := 'customer=entity'
  else             // TFORMMcustomer
    MasterKeyFields := 'customerCode=entity;entityType';
    
  DataPacketName := 'EntityDocs';
  PropertyName := 'docs';
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := True;
  AvailableActions := 'ADD;DELETE';
    
  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
    // Campos Hidden ...........................................................
    DefineHiddenFields('HIDE_ALL_FIELDS');
    // Ordem Campos ............................................................
    DefineOrderFields('tpDoc; nCopies; defMethod; addressNum; Email; Fax; ' +
    	' name; addText; lastUpd; updBy'); // 07-06-2011, # 8943 novos campos
    // Key Fields ..............................................................
    KeyFields:= 'tpDoc;defMethod';
    // Custom Editors ..........................................................
    AddCustomField('tpDoc', 'cxICBtypeDoc');
    AddCustomField('defMethod', 'cxICBdefMethd');
    AddCustomField('addressNum', 'cxEDTaddressNum');
    AddCustomField('nCopies', 'cxEDTnCopies');
    AddCustomField('Email', 'cxEDTemail');
    AddCustomField('Fax', 'cxEDTFax');
  end; //with
  ColsWidthInGrid := '80;75;90;80;450;100;200;150;100;80'; // 07-06-2011, # 8943 novos campos
  
  FdefaultsLoaded := False;
//  UseColsBestFit := False;

  m_GetTypeDoc(cxICBtypeDoc);        //JAR #8593       04-02-2011
  m_GetConfType(cxICBdefMethd);      //JAR #9421/9422  29-04-2011    
  CDStable.Tag := 10;
  DStable.Tag := 10;

```

#### **FRdocumentsInformation.dfm**

```
inherited FRAMEdocumentsInformation: TFRAMEdocumentsInformation
  ParentFont = True
  inherited cxSTLR: TcxStyleRepository
    inherited cxSTLReadOnly: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLDefault: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLInactive: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLgroupBox: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLheader: TcxStyle
      Font.Name = 'Verdana'
    end
    inherited cxSTLselection: TcxStyle
      Font.Name = 'Verdana'
    end
  end
  inherited cxEDTR: TcxEditRepository
    object cxICBdefMethd: TcxEditRepositoryImageComboBoxItem
      Properties.Items = <
        item
          Description = 'MAIL'
          ImageIndex = 0
          Value = 'MAIL'
        end
        item
          Description = 'FAX'
          Value = 'FAX'
        end
        item
          Description = 'POSTAL'
          Value = 'POSTAL'
        end>
    end
    object cxEDTaddressNum: TcxEditRepositoryMaskItem
      Properties.CharCase = ecUpperCase
      Properties.MaskKind = emkRegExpr
      Properties.EditMask = '\d{1,2}'
    end
    object cxEDTFax: TcxEditRepositoryMaskItem
      Properties.CharCase = ecUpperCase
      Properties.MaskKind = emkRegExpr
      Properties.EditMask = '(\+\d{1,4})?\d{1,15}'
    end
    object cxICBtypeDoc: TcxEditRepositoryImageComboBoxItem
      Properties.Items = <
        item
          Description = 'INV'
          ImageIndex = 0
          Value = 'INV'
        end
        item
          Description = 'OACK'
          Value = 'OACK'
        end
        item
          Description = 'PLIST'
          Value = 'PLIST'
        end>
    end
    object cxEDTnCopies: TcxEditRepositoryMaskItem
      Properties.MaskKind = emkRegExpr
      Properties.EditMask = '\d{1,2}'
    end
    object cxEDTemail: TcxEditRepositoryMaskItem
      Properties.MaxLength = 300
    end
  end
end
```
<!-- tabs:end -->

