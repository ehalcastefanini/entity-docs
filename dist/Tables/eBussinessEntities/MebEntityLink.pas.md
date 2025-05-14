<!-- tabs:start -->

#### **Documentation**

# Documentação do Código: Unidade `MebEntityLink`

## 1. Visão Geral:

* **Objetivo Principal:**  
  O objetivo principal deste código é gerenciar a interface de edição de links de entidades de negócios (EBusiness Entity Link). Ele permite que os usuários visualizem, editem e salvem informações relacionadas a entidades e seus vínculos, garantindo que os dados originais sejam preservados para operações de modificação.

* **Tecnologias Utilizadas:**  
  - Delphi (Object Pascal) para desenvolvimento da interface e lógica de negócios.
  - Componentes visuais como `TsPanel`, `TFRAMEebEntityLink`, e `TFRAMEebEntityLinkOldValue` para construção da interface.
  - Serviços utilitários como `TEbEntityLinkServiceUtils` para manipulação de dados.

* **Forma do Componente:**  
  Este código implementa um **formulário** com os seguintes elementos:
  - **Elementos do Formulário:**
    - Comboboxes (`TcxDBImageComboBox`) para seleção de tipos de entidade e partes.
    - Painéis (`TsPanel`) para organização visual.
    - Frames (`TFRAMEebEntityLink` e `TFRAMEebEntityLinkOldValue`) para exibição e edição de dados.
  - **Ações do Formulário:**
    - Carregar dados de uma entidade.
    - Editar e salvar informações de vínculo de entidade.

## 2. Descrição da Funcionalidade:

* **Ações Disponíveis:**
  - Criar o formulário de edição.
  - Carregar dados de uma entidade e seus vínculos.
  - Preservar os dados originais para operações de modificação.

* **Componentes Principais:**
  - `TFORMMebEntityLink`: Classe principal que gerencia o formulário.
  - `TFRAMEebEntityLink` e `TFRAMEebEntityLinkOldValue`: Frames que exibem e manipulam os dados.
  - `TEbEntityLinkServiceUtils`: Serviço utilitário para manipulação de dados.

* **Tradução para Pseudo-código:**
  - Evento `m_CreateFormEdit`:  
    ```pseudo
    se formulário for criado então inicializar componentes
    ```
  - Evento `m_getData`:  
    ```pseudo
    se dados forem carregados então
      configurar parâmetros do serviço
      carregar dados originais e alterados
    fim
    ```

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do formulário (`m_CreateFormEdit`).
  2. Carregamento de dados da entidade e configuração de parâmetros do serviço (`m_getData`).
  3. Exibição dos dados nos frames `TFRAMEebEntityLink` e `TFRAMEebEntityLinkOldValue`.

* **Dados Necessários:**
  - Tipo de entidade (`EntityTp`).
  - Tipo de parte (`ebPartyTp`).
  - Identificador da parte (`ebParty`).
  - Identificador da entidade (`entity`).

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Ação: Carregar dados.  
    Pré-condição: Os parâmetros da entidade devem estar definidos.
  - Ação: Salvar dados.  
    Pré-condição: Os dados devem ser válidos e completos.

* **Filtros Disponíveis:**
  - Mostrar inativos (`ShowInactives`).

* **Mensagens de Erro:**
  - "Parâmetros insuficientes" se os parâmetros necessários não forem fornecidos.

* **Valores Padrão dos Campos:**
  - `ebEntityType`: vazio.
  - `ebPartyTp`: vazio.
  - `ebParty`: vazio.
  - `entityTp`: vazio.
  - `entity`: vazio.

* **Validações e Condições dos Campos:**
  - `EntityTp`: Deve ser preenchido.
  - `ebPartyTp`: Deve ser preenchido.
  - `ebParty`: Deve ser preenchido.
  - `entity`: Deve ser preenchido.

## 5. Funções Principais:

* `m_CreateFormEdit`: Cria e inicializa o formulário de edição.
* `m_getData`: Carrega os dados da entidade e configura os parâmetros do serviço.

## 6. Consumo de Serviços de API:

* **Chamadas a Serviços Externos:**
  - Serviço: `TEbEntityLinkServiceUtils`.
  - Dados enviados: `{ "ebEntityType": "string", "ebPartyTp": "string", "ebParty": "string", "entityTp": "string", "entity": "string" }`.
  - Dados recebidos: `{ "status": "success", "data": "Entity Link object" }`.
  - Propósito: Carregar e salvar dados de vínculo de entidade.
  - Tratamento de erros: Exibe mensagem de erro em caso de falha.

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos definidos no código.

## 8. Dependências:

* **Bibliotecas Externas:**
  - `kneUtils`: Utilitários genéricos.
  - `EbEntityLinkServiceUtils`: Manipulação de dados de vínculo de entidade.

* **Componentes Customizados:**
  - `TFRAMEebEntityLink` e `TFRAMEebEntityLinkOldValue`: Frames para exibição e edição de dados.

## 9. Listagem de Campos e Validações:

* `EntityTp` (tipo: string, obrigatório).
* `ebPartyTp` (tipo: string, obrigatório).
* `ebParty` (tipo: string, obrigatório).
* `entity` (tipo: string, obrigatório).

## 10. Exemplos e Diagramas:

* **Diagrama de Fluxo:**  
  Não aplicável.

* **Diagrama de Sequência:**  
  Não aplicável.

* **Exemplo de Código:**  
  ```pascal
  var
    Form: TFORMMebEntityLink;
  begin
    Form := TFORMMebEntityLink.Create(nil);
    try
      Form.m_getData;
      Form.ShowModal;
    finally
      Form.Free;
    end;
  end;
  ```

* **HTML Representando o Formulário:**  
  ```html
  <div style="width: 652px; height: 315px; border: 1px solid #000;">
    <div style="height: 41px; background-color: #f0f0f0;">Toolbar</div>
    <div style="height: 247px; background-color: #ffffff;">
      <div style="height: 245px; border: 1px solid #ccc;">FRAMEebEntityLink</div>
      <div style="height: 245px; border: 1px solid #ccc;">FRAMEebEntityLinkOldValue</div>
    </div>
  </div>
  ```

## 11. Comentários Importantes no Código:

* O método `m_getData` contém lógica para carregar dados originais e alterados, garantindo que os dados sejam preservados durante modificações.

## 12. Conclusão:

O código implementa um formulário robusto para gerenciar vínculos de entidades de negócios. Ele é bem estruturado, mas depende de serviços externos para manipulação de dados. Uma limitação é a falta de validações explícitas para os campos no código.

## 13. Resumo Curto:

O código gerencia um formulário para edição de vínculos de entidades de negócios, permitindo carregar, editar e salvar dados. Ele utiliza frames e serviços externos para manipulação de dados, garantindo a preservação de registros originais durante modificações.#### **MebEntityLink.pas**

```
unit MebEntityLink;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBEdit, ActnList, ImgList, knePrivileges, StdCtrls, Buttons,
  sBitBtn, sSpeedButton, ToolWin, ComCtrls, acCoolBar, ExtCtrls, sPanel,
  kneEnterAsTab, kneFREditSOA, kneFRCtrlEditSOA, FRebEntityLink,
  FRebEntityLinkOldValue;

type
  TFORMMebEntityLink = class(TFORMkneBaseEdit)
    PNLbackground: TsPanel;
    FRAMEebEntityLink1: TFRAMEebEntityLink;
    FRAMEebEntityLinkOldValue1: TFRAMEebEntityLinkOldValue;
  private
    { Private declarations }
  public
    { Public declarations }

    class function m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit; override;
    procedure m_getData; override;
  end;

var
  FORMMebEntityLink: TFORMMebEntityLink;

implementation

{$R *.dfm}

uses
  kneUtils,
  //--- ServiceUtils
  EbEntityLinkServiceUtils;

class function TFORMMebEntityLink.m_CreateFormEdit(
  const AOwner: TComponent): TFORMkneBaseEdit;
begin
	// Substituir pelo nome do form
  Result := TFORMMebEntityLink.Create(AOwner);
end;




procedure TFORMMebEntityLink.m_getData;
var
  lv_MasterFrame: TFRAMEBaseEditSOA;
  lv_params : TStringList;
begin

  Screen.Cursor := crHourGlass;
  // optimiza��o de recursos
  lv_MasterFrame := TFRAMEBaseEditSOA(kneUtils.TkneGeneric.fg_GetMasterFrame(Self));

  //Neste Form � utilizado para permitir guardar o registo original, em MODIFY,
  // para que o servi�o possa apagar o reg.Original e depois gravar o registo alterado


  FRAMEebEntityLinkOldValue1.MasterSource := lv_MasterFrame.DStable;

  // parametros standard de servi�os
  lv_MasterFrame.ServiceParams.ShowInactives := True;
  lv_params := nil;
  try
    lv_params := TStringList.Create();
    TkneGeneric.SplitString(mv_KeyValues, lv_params, ';');

    with TEbEntityLinkServiceUtils(lv_MasterFrame.ProviderService).Params do
    begin
      if lv_params.Count > 4 then
      begin
        ebEntityType := lv_params.Strings[0];
        ebPartyTp := lv_params.Strings[1];
        ebParty := lv_params.Strings[2];
        entityTp := lv_params.Strings[3];
        entity := lv_params.Strings[4];
      end else
      begin
        ebEntityType := '';
        ebPartyTp := '';
        ebParty := '';
        entityTp  := '';
        entity := '';
      end;
    end;

  finally
    if Assigned(lv_params) then FreeAndNil(lv_params);
  end;

  inherited m_getData;

end;


end.
```

#### **MebEntityLink.dfm**

```
inherited FORMMebEntityLink: TFORMMebEntityLink
  Left = 519
  Top = 336
  Width = 652
  Height = 315
  Caption = 'EBusiness Entity Link'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited PNLtoolbar: TsPanel
    Width = 644
    inherited CLBactions: TsCoolBar
      Width = 644
      Bands = <
        item
          Control = PNbotoes
          ImageIndex = -1
          MinHeight = 41
          Width = 640
        end>
      inherited PNbotoes: TsPanel
        Width = 627
      end
    end
  end
  object PNLbackground: TsPanel [2]
    Left = 0
    Top = 41
    Width = 644
    Height = 247
    Align = alClient
    TabOrder = 1
    SkinData.SkinSection = 'PANEL'
    inline FRAMEebEntityLinkOldValue1: TFRAMEebEntityLinkOldValue
      Left = 1
      Top = 1
      Width = 642
      Height = 245
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 1
      inherited PNLfooter: TsPanel
        Top = 211
        Width = 642
      end
    end
    inline FRAMEebEntityLink1: TFRAMEebEntityLink
      Left = 1
      Top = 1
      Width = 642
      Height = 245
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
      inherited PNLfooter: TsPanel
        Top = 211
        Width = 642
      end
      inherited PNL2: TsPanel
        Width = 642
        inherited ICBOentity_tp: TcxDBImageComboBox
          DataBinding.DataField = 'EntityTp'
          Properties.OnEditValueChanged = nil
          Width = 117
        end
        inherited ICBOeb_entity_type: TcxDBImageComboBox
          Width = 117
        end
        inherited ICBOeb_Party_Tp: TcxDBImageComboBox
          Width = 205
        end
      end
    end
  end
end
```
<!-- tabs:end -->

